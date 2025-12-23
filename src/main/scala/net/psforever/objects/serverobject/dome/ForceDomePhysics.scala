// Copyright (c) 2025 PSForever
package net.psforever.objects.serverobject.dome

import net.psforever.objects.serverobject.structures.Amenity
import net.psforever.objects.serverobject.terminals.capture.CaptureTerminalAware
import net.psforever.types.Vector3

class ForceDomePhysics(private val cfddef: ForceDomeDefinition)
  extends Amenity
    with CaptureTerminalAware {
  private var energized: Boolean = false

  private var perimeter: List[(Vector3, Vector3)] = List()

  def Energized: Boolean = energized

  def Energized_=(state: Boolean): Boolean = {
    energized = state
    Energized
  }

  def Perimeter: List[(Vector3, Vector3)] = perimeter

  def Perimeter_=(list: List[(Vector3, Vector3)]): List[(Vector3, Vector3)] = {
    perimeter = list
    Perimeter
  }

  def Definition: ForceDomeDefinition = cfddef
}

object ForceDomePhysics {
  import akka.actor.ActorContext

  /**
   * Instantiate and configure a `CapitolForceDome` object.
   * @param pos position of the object in the zone's coordinate system
   * @param fddef specific type of force dome
   * @param id the unique id that will be assigned to this entity
   * @param context a context to allow the object to properly set up `ActorSystem` functionality
   * @return the `CapitolForceDome` object
   */
  def Constructor(pos: Vector3, fddef: ForceDomeDefinition)(id: Int, context: ActorContext): ForceDomePhysics = {
    import akka.actor.Props

    val obj = new ForceDomePhysics(fddef)
    obj.Position = pos
    obj.Actor = context.actorOf(Props(classOf[ForceDomeControl], obj), name = s"${fddef.Name}_$id")
    obj
  }
}
