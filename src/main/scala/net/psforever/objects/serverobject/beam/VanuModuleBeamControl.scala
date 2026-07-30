// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.beam

import akka.actor.{Actor, Cancellable}
import net.psforever.objects.{Default, Player}
import net.psforever.objects.serverobject.CommonMessages
import net.psforever.objects.serverobject.damage.Damageable
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.objects.sourcing.SourceEntry
import net.psforever.objects.vital.Vitality
import net.psforever.objects.vital.etc.VanuModuleBeamReason
import net.psforever.objects.vital.interaction.DamageInteraction

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class VanuModuleBeamControl(beam: VanuModuleBeam) extends Actor {
  private val cachedBeamDamageReason = VanuModuleBeamReason(beam)
  private val cachedBeamPosition = beam.Position

  private var subjectOfCharging: Option[String] = None
  private var chargeTimer: Cancellable = Default.Cancellable
  private val safeInteractionCooldowns: mutable.HashMap[String, Cancellable] = new mutable.HashMap()

  override def postStop(): Unit = {
    super.postStop()
    subjectOfCharging = None
    chargeTimer.cancel()
    safeInteractionCooldowns.foreach { case (_, event) => event.cancel() }
    safeInteractionCooldowns.clear()
  }

  val receive: Receive = {
    case CommonMessages.Use(player, _)
      if safeInteractionCooldowns.contains(player.Name) =>
      //redundant interactions while already exposed to beam
      ()

    case CommonMessages.Use(player, Some(module: VanuModule))
      if subjectOfCharging.isEmpty && !module.Charged =>
      //todo charge pose
      subjectOfCharging = Some(player.Name)
      chargeTimer = context.system.scheduler.scheduleOnce(5000 milliseconds, self, VanuModuleBeamControl.ChargeComplete(player, module))
      player.Actor ! Damageable.MakeInvulnerable
      safeInteractionCooldowns.addOne(player.Name, Default.Cancellable)

    case VanuModuleBeamControl.ChargeComplete(player, module) =>
      //todo undo charge pose
      subjectOfCharging = None
      chargeTimer.cancel()
      module.Charged = true
      player.Actor ! Damageable.MakeVulnerable
      safeInteractionCooldowns.update(
        player.Name,
        context.system.scheduler.scheduleOnce(5000 milliseconds, self, VanuModuleBeamControl.ExpiredInteractionProtection(player.Name))
      )

    //todo bfr imprinting

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
}

object VanuModuleBeamControl {
  private case class ChargeComplete(player: Player, module: VanuModule)

  private case class ExpiredInteractionProtection(playerName: String)
}
