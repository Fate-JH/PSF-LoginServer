// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.Player
import net.psforever.objects.serverobject.structures.{Amenity, PoweredAmenityControl}
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAware, CaptureTerminalAwareBehavior}
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.PlanetSideEmpire

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
    TryDestroyModuleInCradle()
  }

  override def captureTerminalIsResecured(terminal: CaptureTerminal): Unit = {
    //if the module is no longer faction-aligned with the cradle/facility, destroy it; no freebies
    //if the facility has gone neutral, destroy it regardless of faction-alignment
    obj.captureFlag.collect { case flag: VanuModule
      if flag.FlagFaction != obj.Faction || terminal.Faction == PlanetSideEmpire.NEUTRAL =>
      TryDestroyModuleInCradle(flag)
    }
  }

  private def TryDestroyModuleInCradle(): Unit = {
    obj.captureFlag.collect { case module: VanuModule =>
      TryDestroyModuleInCradle(module)
    }
  }

  private def TryDestroyModuleInCradle(module: VanuModule): Unit = {
    obj.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.ModuleLost(module))
  }

  private def TryFreeModuleFromCradle(): Unit = {
    obj.captureFlag.foreach { _ =>
      obj.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.FreeModuleFromCradle(obj))
    }
  }
}
