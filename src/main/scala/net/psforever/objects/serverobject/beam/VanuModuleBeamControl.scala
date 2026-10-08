// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.beam

import akka.actor.{Actor, Cancellable}
import net.psforever.objects.Player
import net.psforever.objects.serverobject.CommonMessages
import net.psforever.objects.serverobject.damage.Damageable
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.objects.sourcing.SourceEntry
import net.psforever.objects.vital.Vitality
import net.psforever.objects.vital.etc.VanuModuleBeamReason
import net.psforever.objects.vital.interaction.DamageInteraction
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class VanuModuleBeamControl(beam: VanuModuleBeam) extends Actor {
  private val cachedBeamDamageReason = VanuModuleBeamReason(beam)
  private val cachedBeamPosition = beam.Position

  private var subjectOfCharging: Option[Player] = None
  private val safeInteractionCooldowns: mutable.HashMap[String, Cancellable] = new mutable.HashMap()

  override def postStop(): Unit = {
    super.postStop()
    RemoveUser()
    safeInteractionCooldowns.foreach { case (_, event) => event.cancel() }
    safeInteractionCooldowns.clear()
  }

  val receive: Receive = {
    case CommonMessages.Use(player, _)
      if safeInteractionCooldowns.contains(player.Name) =>
        //redundant interaction while already exposed to beam
      safeInteractionCooldowns.get(player.Name).map(_.cancel())
      safeInteractionCooldowns.update(
        player.Name,
        context.system.scheduler.scheduleOnce(5000 milliseconds, self, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
      )

    case CommonMessages.Use(player, Some(module: VanuModule))
      if subjectOfCharging.isEmpty && !module.Charged.value =>
      AddUser(player)
      safeInteractionCooldowns.addOne(
        player.Name,
        context.system.scheduler.scheduleOnce(5000 milliseconds, self, VanuModuleBeamControl.ChargeComplete(player, module))
      )

    case VanuModuleBeamControl.ChargeComplete(player, module)
      if subjectOfCharging.contains(player) =>
      RemoveUser()
      beam.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.ChargeFlag(module))
      safeInteractionCooldowns.update(
        player.Name,
        context.system.scheduler.scheduleOnce(5000 milliseconds, self, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
      )

    //todo bfr imprinting? might be a separate beam

    case VanuModuleBeamControl.ExpiredInteractionProtection(name) =>
      safeInteractionCooldowns.remove(name).map(_.cancel())

    case CommonMessages.Use(player, _) =>
      //no excuse to be interacting with the beam safely
      player.Actor ! Vitality.Damage(
        DamageInteraction(
          SourceEntry(player),
          cachedBeamDamageReason,
          cachedBeamPosition
        ).calculate()
      )

    case _ => ()
  }

  private def AddUser(player: Player): Unit = {
    safeInteractionCooldowns.get(player.Name).foreach(_.cancel())
    subjectOfCharging = Some(player)
    beam.AddUser(player)
    //todo charge pose
    player.Actor ! Damageable.MakeInvulnerable
  }

  private def RemoveUser(): Unit = {
    subjectOfCharging.foreach { p =>
      safeInteractionCooldowns.remove(p.Name).foreach(_.cancel())
      subjectOfCharging = None
      beam.RemoveUser(p)
      //todo undo charge pose
      p.Actor ! Damageable.MakeVulnerable
    }
  }
}

object VanuModuleBeamControl {
  private case class ChargeComplete(player: Player, module: VanuModule)

  private case class ExpiredInteractionProtection(playerName: String)
}
