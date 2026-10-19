// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.Actor
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAwareBehavior}
import net.psforever.services.base.envelope.MessageEnvelope
import net.psforever.services.local.LocalAction
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.{PlanetSideEmpire, Vector3}

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

  override protected def captureTerminalIsHacked(@unused terminal: CaptureTerminal): Unit = {
    if (terminal.HackedBy.isEmpty) {
      //our faction identity was manually set, this is not an actual facility hack
    }
  }

  override protected def captureTerminalIsResecured(@unused terminal: CaptureTerminal): Unit = {
    val zone = obj.Zone
    obj.captureFlag match {
      case Some(flag: VanuModule)
        if flag.Faction != terminal.Faction && flag.Zone == zone =>
        //module is out in the cavern, hack was completed
        flag.Destroyed = true
        flag.Carrier match {
          case Some(carrier) =>
            flag.Position = carrier.Position + Vector3.z(value = 0.5f)
            //todo server-side explosion, or damage just to carrier?
          case None => ()
            zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.LluSpawned(flag)) //explosion animation
        }
        zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.FlagLost(flag))
      case None
        if terminal.Faction != PlanetSideEmpire.NEUTRAL =>
        //spawn module attempt
        //! TrySpawnModule()
      case None | Some(_) => () //either module is in another zone, or the defenders resecured a hacked facility
    }
  }
}
