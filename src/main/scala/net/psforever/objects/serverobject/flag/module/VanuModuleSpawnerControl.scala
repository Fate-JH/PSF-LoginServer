// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.Actor
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAwareBehavior}
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.PlanetSideEmpire

import scala.annotation.unused
//import scala.concurrent.ExecutionContext.Implicits.global

object VanuModuleSpawnerControl {
  case object ClearModule
}

class VanuModuleSpawnerControl(obj: VanuModuleNode)
  extends Actor
    with CaptureTerminalAwareBehavior {
  def CaptureTerminalAwareObject: VanuModuleNode = obj

  val receive: Receive = captureTerminalAwareBehaviour.orElse { _ => () }

  override protected def captureTerminalIsResecured(@unused terminal: CaptureTerminal): Unit = {
    val zone = obj.Zone
    obj.captureFlag match {
      case Some(flag: VanuModule)
        if terminal.Faction != flag.FlagFaction || terminal.Faction == PlanetSideEmpire.NEUTRAL =>
        zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.ModuleLost(flag))
      case _ =>
        ()
    }
  }
}
