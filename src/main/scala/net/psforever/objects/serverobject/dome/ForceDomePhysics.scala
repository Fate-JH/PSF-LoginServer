// Copyright (c) 2025 PSForever
package net.psforever.objects.serverobject.dome

import net.psforever.objects.serverobject.structures.Amenity
import net.psforever.types.Vector3

class ForceDomePhysics(private val cfddef: ForceDomeDefinition)
  extends Amenity {
  private var energized: Boolean = false

  def Energized: Boolean = energized

  def Energized_=(state: Boolean): Boolean = {
    energized = state
    Energized
  }

  def Definition: ForceDomeDefinition = cfddef
}

object ForceDomePhysics {
  import akka.actor.ActorContext

  /**
   * Instantiate and configure a `CapitolForceDome` object.
   * @param pos positon of the object in the zone's coordinate system
   * @param id the unique id that will be assigned to this entity
   * @param context a context to allow the object to properly set up `ActorSystem` functionality
   * @return the `CapitolForceDome` object
   */
  def Constructor(pos: Vector3)(id: Int, context: ActorContext): ForceDomePhysics = {
    //import akka.actor.Props
    import net.psforever.objects.GlobalDefinitions

    val obj = new ForceDomePhysics(GlobalDefinitions.force_dome_generator)
    obj.Position = pos
    //obj.Actor = context.actorOf(Props(classOf[null], obj), s"${GlobalDefinitions.door.Name}_$id")
    obj
  }
}
