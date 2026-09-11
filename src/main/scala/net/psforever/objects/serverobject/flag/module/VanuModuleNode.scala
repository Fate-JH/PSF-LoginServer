// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.{ActorContext, Props}
import net.psforever.objects.serverobject.flag.base.{CarriableFlag, FlagSocket, FlagSocketDefinition}
import net.psforever.objects.serverobject.terminals.capture.CaptureTerminalAware
import net.psforever.objects.zones.ZoneAware
import net.psforever.types.Vector3

class VanuModuleNode(tDef: FlagSocketDefinition)
  extends FlagSocket
    with CaptureTerminalAware
    with ZoneAware {
  def ValidFlagType: CarriableFlag = captureFlag.getOrElse(Definition).ValidFlagType

  def Definition : FlagSocketDefinition = tDef
}

object VanuModuleNode {
  def apply(tDef: FlagSocketDefinition) : VanuModuleNode = {
    new VanuModuleNode(tDef)
  }

  def Constructor(tdef: FlagSocketDefinition, pos: Vector3)(id: Int, context: ActorContext): VanuModuleNode = {
    val obj = VanuModuleNode(tdef)
    obj.Position = pos
    obj.Actor = context.actorOf(Props(classOf[VanuModuleCradleControl], obj), s"${obj.Definition.Name}_$id")
    obj
  }
}

object VanuModuleSpawn {
  def apply(tDef: FlagSocketDefinition) : VanuModuleNode = {
    new VanuModuleNode(tDef)
  }

  def Constructor(tdef: FlagSocketDefinition, pos: Vector3)(id: Int, context: ActorContext): VanuModuleNode = {
    val obj = VanuModuleNode(tdef)
    obj.Position = pos
    obj.Actor = context.actorOf(Props(classOf[VanuModuleSpawnerControl], obj), s"${obj.Definition.Name}_$id")
    obj
  }
}
