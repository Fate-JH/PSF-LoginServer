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
  def ValidFlagType: CarriableFlag = Definition.ValidFlagType

  override def DisplayFlagType: CarriableFlag = captureFlag.getOrElse(Definition).ValidFlagType

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

  trait OriginalSpawner {
    def ClearModule(): Option[VanuModule]
  }

  case class WithModuleInstalled(node: VanuModuleNode, module: VanuModule) extends OriginalSpawner {
    def ClearModule(): Option[VanuModule] = {
      node.captureFlag match {
        case Some(flag) if flag == module =>
          node.captureFlag = None
          Some(module)
        case _ =>
          None
      }
    }
  }

  case class WithDifferentModuleInstalled(node: VanuModuleNode) extends OriginalSpawner {
    def ClearModule(): Option[VanuModule] = None
  }

  case class WithoutModuleInstalled(node: VanuModuleNode) extends OriginalSpawner {
    def ClearModule(): Option[VanuModule] = None
  }

  case object NotFound extends OriginalSpawner {
    def ClearModule(): Option[VanuModule] = None
  }

  /**
   * Trace back a Vanu module to the spawner that produced it originally.
   * This will probably not work if the module has already been charged
   * as that process may involve blanking the spawning cradle's knowledge of the module
   * to stop it from asserting control over the module.<br>
   * If the original `Owner` facilitates both spawner nodes and receptacle nodes,
   * this function will still return the spawner node.
   * The receptacle node should be distinguished by being a fundamentally different type
   * than the module (that can be) installed.<br>
   * Repetition of this function is descriptive: (find the) `VanuModuleNode.ThatSpawnedThis(module)`.
   * @see `Building.Amenities`
   * @see `VanuModuleNode.ValidFlagType`
   * @param module vanu module
   * @return module node that spawned the module, if it can be determined
   */
  def ThatSpawnedThis(module: VanuModule): OriginalSpawner = {
    module
      .Owner
      .Amenities
      .collectFirst { case o: VanuModuleNode if o.ValidFlagType == module.ValidFlagType => o }
      .map { o =>
        if (o.captureFlag.contains(module)) {
          WithModuleInstalled(o, module)
        } else if (o.captureFlag.isEmpty) {
          WithoutModuleInstalled(o)
        } else {
          WithDifferentModuleInstalled(o)
        }
      }
      .getOrElse(NotFound)
  }
}

object VanuModuleSpawn {
  def Constructor(tdef: FlagSocketDefinition, pos: Vector3)(id: Int, context: ActorContext): VanuModuleNode = {
    val obj = VanuModuleNode(tdef)
    obj.Position = pos
    obj.Actor = context.actorOf(Props(classOf[VanuModuleSpawnerControl], obj), s"${obj.Definition.Name}_$id")
    obj
  }
}
