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

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object VanuModuleBeamControl {
  final val ChargingInterval: Long = 250L
  final val ChargingDuration: Long = 5000L
  final val ProtectionDuration: Long = 5000L

  case class ChargeEnd(player: Player, module: VanuModule)

  case class ChargeFailed(player: Player, module: VanuModule)

  case class ChargeComplete(player: Player, module: VanuModule)

  private case class ExpiredInteractionProtection(playerName: String)
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

  private val startChargingBehavior: Receive = {
    case CommonMessages.Use(player, Some(module: VanuModule))
      if subjectOfCharging.isEmpty &&
        VanuModuleManager.PlayerModuleChargingTests(player, beam, module) =>
      //start charging module
      AddUser(player)
      beam.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.StartChargingModule(module, beam, player))
      context.become(chargingBehavior)

    //todo bfr imprinting? might be a separate beam
  }

  private val justChargingBehavior: Receive = {
      case CommonMessages.Use(_, _) =>
        //interaction while ongoing charge; protectedBehavior will ensure the appropriate carrier can get to this point
        ()

      case VanuModuleBeamControl.ChargeEnd(player, _) =>
        //stopped charging
        RemoveUser()
        RetimeRecontextEvent(player.Name, VanuModuleBeamControl.ProtectionDuration, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
        context.become(beforeChargingBehavior)

      case VanuModuleBeamControl.ChargeFailed(player, _) =>
        //stopped charging
        RemoveUser()
        HardStopCurrentEvent(player.Name)
        context.become(beforeChargingBehavior)

      case VanuModuleBeamControl.ChargeComplete(player, _) =>
        //actually charged module
        RemoveUser()
        RetimeRecontextEvent(player.Name, VanuModuleBeamControl.ProtectionDuration, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
        context.become(beforeChargingBehavior)
    }

  private val beforeChargingBehavior: Receive =
    protectedBehavior
      .orElse(startChargingBehavior)
      .orElse(notProtectedBehavior)
      .orElse {
        case _ => ()
      }

  private val chargingBehavior: Receive =
    protectedBehavior
      .orElse(justChargingBehavior)
      .orElse(notProtectedBehavior)
      .orElse {
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
    safeInteractionCooldowns.update(
      label,
      context.system.scheduler.scheduleOnce(interval milliseconds, self, newMessage)
    )
  }
}
