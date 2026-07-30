// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.beam

import akka.actor.ActorContext
import net.psforever.objects.serverobject.structures.Amenity
import net.psforever.types.Vector3

class VanuModuleBeam(mDef: VanuModuleBeamDefinition) extends Amenity {
  override def Definition: VanuModuleBeamDefinition = mDef
}

object VanuModuleBeam {
  /**
   * Overloaded constructor.
   * @param vmDef the `ObjectDefinition` that constructs this object and maintains some of its immutable fields
   * @return a `VanuModuleBeam` object
   */
  def apply(vmDef: VanuModuleBeamDefinition): VanuModuleBeam = {
    new VanuModuleBeam(vmDef)
  }

  /**
   * Instantiate and configure a `VanuModuleBeam` object
   * @param id the unique id that will be assigned to this entity
   * @param context a context to allow the object to properly set up `ActorSystem` functionality
   * @return the `VanuModuleBeam` object
   */
  def Constructor(pos: Vector3, vmDef: VanuModuleBeamDefinition)(id: Int, context: ActorContext): VanuModuleBeam = {
    import akka.actor.Props
    val obj = new VanuModuleBeam(vmDef)
    obj.Position = pos
    obj.Actor = context.actorOf(Props(classOf[VanuModuleBeamControl], obj), s"${vmDef.Name}_$id")
    obj
  }
}
