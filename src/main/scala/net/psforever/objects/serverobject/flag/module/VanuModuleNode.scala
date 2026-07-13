// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.{ActorContext, Props}
import net.psforever.objects.GlobalDefinitions
import net.psforever.objects.serverobject.flag.base.{FlagDefinition, FlagSocket, FlagSocketDefinition, FlagType}
import net.psforever.objects.serverobject.flag.llu.{CaptureFlagSocket, CaptureFlagSocketControl}
import net.psforever.types.Vector3

class VanuModuleNode(tDef: FlagSocketDefinition)
  extends FlagSocket {
  def ValidFlagType: FlagType = captureFlag.getOrElse(Definition).ValidFlagType

  def Definition : FlagSocketDefinition = tDef
}

object VanuModuleNode {
  def apply(tDef: FlagSocketDefinition) : VanuModuleNode = {
    new VanuModuleNode(tDef)
  }

  def Constructor(pos: Vector3)(id: Int, context: ActorContext) : VanuModuleNode = {
    Constructor(GlobalDefinitions.llm_socket, pos)(id, context)
  }

  def Constructor(tdef: FlagSocketDefinition, pos: Vector3)(id: Int, context: ActorContext): VanuModuleNode = {
    val obj = VanuModuleNode(tdef)
    obj.Position = pos
    //obj.Actor = context.actorOf(Props(classOf[CaptureFlagSocketControl], obj), s"${obj.Definition.Name}_$id")
    obj
  }
}
