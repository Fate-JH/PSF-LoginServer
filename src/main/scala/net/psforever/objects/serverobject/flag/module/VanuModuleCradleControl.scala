// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.Player
import net.psforever.objects.serverobject.structures.{Amenity, PoweredAmenityControl}
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAware, CaptureTerminalAwareBehavior}
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.PlanetSideEmpire

import scala.annotation.unused

object VanuModuleCradleControl {
  final case class InstallModule(flag: VanuModule, player: Player)

  case object FreeModule
}

class VanuModuleCradleControl(obj: VanuModuleNode)
  extends PoweredAmenityControl
    with CaptureTerminalAwareBehavior {
  def CaptureTerminalAwareObject: Amenity with CaptureTerminalAware = obj

  def poweredStateLogic: Receive = captureTerminalAwareBehaviour.orElse {
    case VanuModuleCradleControl.InstallModule(flag, player) =>
      obj.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.InstallModuleIntoCradle(obj, flag, player))

    case VanuModuleCradleControl.FreeModule =>
      TryFreeModuleFromCradle()

    case _ => ()
  }

  def unpoweredStateLogic: Receive = {
    case _ => ()
  }

  def powerTurnOnCallback(): Unit = { /* nothing */ }

  def powerTurnOffCallback(): Unit = {
    TryFreeModuleFromCradle()
  }

  override def captureTerminalIsResecured(terminal: CaptureTerminal): Unit = {
    if (terminal.Faction == PlanetSideEmpire.NEUTRAL) {
      TryFreeModuleFromCradle()
    }
  }

  override protected def captureTerminalIsHacked(@unused terminal: CaptureTerminal): Unit = {
    TryFreeModuleFromCradle()
  }

  private def TryFreeModuleFromCradle(): Unit = {
    obj.captureFlag.foreach { _ =>
      obj.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.FreeModuleFromCradle(obj))
    }
  }
}
