// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.{ActorContext, Props}
import net.psforever.objects.serverobject.flag.base.{CarriableFlag, FlagSocket, FlagSocketDefinition}
import net.psforever.objects.serverobject.structures.{AmenityOwner, Building}
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

  trait FoundModuleCradle {
    def ClearModule(): Option[VanuModule]

    def hasModuleInstalled: Boolean

    def GetSpawner: Option[VanuModuleNode]
  }

  case class WithModuleInstalled(node: VanuModuleNode, module: VanuModule) extends FoundModuleCradle {
    def ClearModule(): Option[VanuModule] = {
      node.captureFlag match {
        case Some(flag) if flag == module =>
          node.captureFlag = None
          Some(module)
        case _ =>
          None
      }
    }

    def hasModuleInstalled: Boolean = node.captureFlag.nonEmpty

    def GetSpawner: Option[VanuModuleNode] = Some(node)
  }

  case class WithDifferentModuleInstalled(node: VanuModuleNode) extends FoundModuleCradle {
    def ClearModule(): Option[VanuModule] = None

    def hasModuleInstalled: Boolean = false

    def GetSpawner: Option[VanuModuleNode] = Some(node)
  }

  case class WithoutModuleInstalled(node: VanuModuleNode) extends FoundModuleCradle {
    def ClearModule(): Option[VanuModule] = None

    def hasModuleInstalled: Boolean = false

    def GetSpawner: Option[VanuModuleNode] = Some(node)
  }

  case object CradleNotFound extends FoundModuleCradle {
    def ClearModule(): Option[VanuModule] = None

    def hasModuleInstalled: Boolean = false

    def GetSpawner: Option[VanuModuleNode] = None
  }

  /**
   * Trace back a Vanu module to the spawner that produced it originally.
   * @see [[ThatSpawnedThis(VanuModule*, AmenityOwner*)]]
   * @param module vanu module being sought
   * @return module node that spawned the module, if it can be determined
   */
  def ThatSpawnedThis(module: VanuModule): FoundModuleCradle = {
    ThatSpawnedThis(module, module.Owner)
  }

  /**
   * Trace back a Vanu module to the spawner that produced it originally.<br>
   * This should not work if the module has already been charged
   * as the spawning cradle's knowledge of the module will be blanked
   * to disallow it from asserting control over the module.<br>
   * If the original `Owner` facilitates both spawner nodes and receptacle nodes,
   * this function will still return the spawner node.
   * The receptacle node should be distinguished by being a fundamentally different type
   * than the module (that can be) installed.<br>
   * Repetition of this function is descriptive: (find the) `VanuModuleNode.ThatSpawnedThis(module)`.
   * @see `Building.Amenities`
   * @see `VanuModuleNode.ValidFlagType`
   * @param module vanu module being sought
   * @param mockOwner location whose amenities will be searched
   * @return module node that spawned the module, if it can be determined; else `CradleNotFound`
   */
  def ThatSpawnedThis(module: VanuModule, mockOwner: AmenityOwner): FoundModuleCradle = {
    mockOwner
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
      .getOrElse(CradleNotFound)
  }

  /**
   * Trace back a Vanu module to the spawner that currently has it installed.
   * Test the current target of installation - where it should be installed - or
   * the owner of the module - where it should have originated.
   * @see [[ThatContainsThis(VanuModule*, AmenityOwner*)]]
   * @see `Building.noBuilding`
   * @see `VanuModule.Target`
   * @param module vanu module being sought
   * @return module node in which the module is installed, if it can be determined
   */
  def ThatContainsThis(module: VanuModule): FoundModuleCradle = {
    if (module.Target != Building.NoBuilding) {
      ThatContainsThis(module, module.Target)
    } else {
      ThatContainsThis(module, module.Owner)
    }
  }

  /**
   * Trace back a Vanu module to the spawner that currently has it installed.
   * @see `Building.Amenities`
   * @see `VanuModuleNode.captureFlag`
   * @param module vanu module being sought
   * @param mockOwner location whose amenities will be searched
   * @return module node in which the module is installed, if it can be determined
   */
  def ThatContainsThis(module: VanuModule, mockOwner: AmenityOwner): FoundModuleCradle = {
    mockOwner
      .Amenities
      .collectFirst { case o: VanuModuleNode if o.captureFlag.contains(module) => o }
      .map(WithModuleInstalled(_, module))
      .getOrElse(CradleNotFound)
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
