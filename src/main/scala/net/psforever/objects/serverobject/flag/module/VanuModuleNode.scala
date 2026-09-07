// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.{ActorContext, Props}
import net.psforever.objects.serverobject.flag.base.{FlagSocket, FlagSocketDefinition, CarriableFlag}
import net.psforever.types.Vector3

class VanuModuleNode(tDef: FlagSocketDefinition)
  extends FlagSocket {
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
