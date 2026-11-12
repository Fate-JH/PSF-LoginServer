// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.beam

import akka.actor.{Actor, Cancellable}
import net.psforever.objects.Player
import net.psforever.objects.serverobject.CommonMessages
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.objects.sourcing.SourceEntry
import net.psforever.objects.vital.Vitality
import net.psforever.objects.vital.etc.VanuModuleBeamReason
import net.psforever.objects.vital.interaction.DamageInteraction
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.Vector3

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object VanuModuleBeamControl {
  final val ChargingInterval: Long = 250L
  final val ChargingDuration: Long = 5000L
  final val ProtectionDuration: Long = 5000L

  private case class ChargeOngoing(player: Player, module: VanuModule, interval: Long, endTime: Long)

  private case class ChargeComplete(player: Player, module: VanuModule)

  private case class ExpiredInteractionProtection(playerName: String)

  private def PlayerModuleChargingTests(player: Player, beam: VanuModuleBeam, module: VanuModule): Boolean = {
    player.isAlive && Vector3.DistanceSquared(player.Position.xy, beam.Position.xy) < 2 * math.pow(beam.Definition.UseRadius, 2) &&
      !module.Charged.value && !module.Destroyed && module.Carrier.contains(player)
  }
}

class VanuModuleBeamControl(beam: VanuModuleBeam) extends Actor {
  private lazy val cachedBeamDamageReason = VanuModuleBeamReason(beam)

  private var subjectOfCharging: Option[Player] = None
  private val safeInteractionCooldowns: mutable.HashMap[String, Cancellable] = new mutable.HashMap()

  override def postStop(): Unit = {
    super.postStop()
    RemoveUser()
    safeInteractionCooldowns.foreach { case (_, event) => event.cancel() }
    safeInteractionCooldowns.clear()
  }

  private val protectedBehavior: Receive = {
    case CommonMessages.Use(player, _)
      if safeInteractionCooldowns.contains(player.Name) && !subjectOfCharging.contains(player) =>
      //not charging but still protected from redundant interaction while exposed to beam
      RetimeRecontextEvent(
        player.Name,
        VanuModuleBeamControl.ProtectionDuration,
        VanuModuleBeamControl.ExpiredInteractionProtection(player.Name)
      )

    case CommonMessages.Use(player, _)
      if safeInteractionCooldowns.contains(player.Name) =>
      () //still protected from redundant interaction while exposed to beam
  }

  private val startChargingBehavior: Receive = {
    case CommonMessages.Use(player, Some(module: VanuModule))
      if subjectOfCharging.isEmpty && VanuModuleBeamControl.PlayerModuleChargingTests(player, beam, module) =>
      //start charging module
      AddUser(player)
      RetimeRecontextEvent(
        player.Name,
        VanuModuleBeamControl.ChargingInterval,
        VanuModuleBeamControl.ChargeOngoing(player, module, VanuModuleBeamControl.ChargingInterval, System.currentTimeMillis() + VanuModuleBeamControl.ChargingDuration)
      )
      beam.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.StartChargingModule(module, beam, player))
      context.become(chargingBehavior)

    //todo bfr imprinting? might be a separate beam
  }

  private val notProtectedBehavior: Receive = {
    case CommonMessages.Use(player, _)
      if player.isAlive =>
      //no excuse to be interacting with the beam safely
      player.Actor ! Vitality.Damage(
        DamageInteraction(
          SourceEntry(player),
          cachedBeamDamageReason,
          beam.Position
        ).calculate()
      )

    case VanuModuleBeamControl.ExpiredInteractionProtection(name) =>
      //no longer protected from beam interactions
      HardStopCurrentEvent(name)
  }

  private val beforeChargingBehavior: Receive =
    protectedBehavior
      .orElse(startChargingBehavior)
      .orElse(notProtectedBehavior)
      .orElse {
        case _ => ()
      }

  private val chargingBehavior: Receive = protectedBehavior
    .orElse(notProtectedBehavior)
    .orElse {
      case msg @ VanuModuleBeamControl.ChargeOngoing(player, module, interval, endTime)
        if System.currentTimeMillis() + interval < endTime && VanuModuleBeamControl.PlayerModuleChargingTests(player, beam, module) =>
        //pass conditions to continue charging module
        RetimeRecontextEvent(player.Name, interval, msg)

      case VanuModuleBeamControl.ChargeOngoing(player, module, interval, endTime)
        if System.currentTimeMillis() + interval >= endTime && VanuModuleBeamControl.PlayerModuleChargingTests(player, beam, module) =>
        //goto finish charging module next
        RetimeRecontextEvent(player.Name, interval, VanuModuleBeamControl.ChargeComplete(player, module))

      case VanuModuleBeamControl.ChargeOngoing(player, module, _, _) =>
        //failed conditions to continue charging
        RemoveUser()
        beam.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.StopChargingModule(module, beam, player))
        context.become(beforeChargingBehavior)

      case VanuModuleBeamControl.ChargeComplete(player, module)
        if VanuModuleBeamControl.PlayerModuleChargingTests(player, beam, module) =>
        //actually charge module
        RemoveUser()
        beam.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.ChargeComplete(module, beam, player))
        RetimeRecontextEvent(player.Name, VanuModuleBeamControl.ProtectionDuration, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
        context.become(beforeChargingBehavior)

      case VanuModuleBeamControl.ChargeComplete(player, _) =>
        //failed to charge module at the very last second
        RemoveUser()
        HardStopCurrentEvent(player.Name)
        context.become(beforeChargingBehavior)

      case _ => ()
    }

  val receive: Receive = beforeChargingBehavior

  private def AddUser(player: Player): Unit = {
    StopCurrentEvent(player.Name)
    subjectOfCharging = Some(player)
    beam.AddUser(player)
  }

  private def RemoveUser(): Unit = {
    subjectOfCharging.foreach { p =>
      StopCurrentEvent(p.Name)
      subjectOfCharging = None
      beam.RemoveUser(p)
    }
  }

  private def HardStopCurrentEvent(label: String): Unit = {
    safeInteractionCooldowns.remove(label).foreach(_.cancel())
  }

  private def StopCurrentEvent(label: String): Unit = {
    safeInteractionCooldowns.get(label).foreach(_.cancel())
  }

  private def RetimeRecontextEvent(label: String, interval: Long, newMessage: Any): Unit = {
    StopCurrentEvent(label)
    safeInteractionCooldowns.addOne(
      label,
      context.system.scheduler.scheduleOnce(interval milliseconds, self, newMessage)
    )
  }
}
