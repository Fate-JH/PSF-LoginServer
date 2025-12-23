// Copyright (c) 2025 PSForever
package net.psforever.objects.serverobject.dome

import net.psforever.objects.geometry.d3.{Sphere, VolumetricGeometry}
import net.psforever.objects.serverobject.structures.AmenityDefinition
import net.psforever.objects.sourcing.SourceEntry
import net.psforever.types.Vector3

class ForceDomeDefinition(objectId: Int)
  extends AmenityDefinition(objectId) {
  Name = "force_dome"
  Geometry = ForceDomeDefinition.representBy

  /** offsets that define the perimeter of the pyramidal force "dome" barrier;
   * these points are the closest to where the dome interacts with the ground at a corner;
   * should be sequential, either clockwise or counterclockwise */
  private var perimeter: List[Vector3] = List()

  def PerimeterOffsets: List[Vector3] = perimeter

  def PerimeterOffsets_=(points: List[Vector3]): List[Vector3] = {
    perimeter = points
    PerimeterOffsets
  }
}

object ForceDomeDefinition {
  /**
   * na
   * @param o na
   * @return na
   */
  def representBy(o: Any): VolumetricGeometry = {
    import net.psforever.objects.geometry.GeometryForm.invalidPoint
    o match {
      case fdp: ForceDomePhysics =>
        Sphere(fdp.Position, fdp.Definition.UseRadius)
      case s: SourceEntry =>
        Sphere(s.Position, s.Definition.UseRadius)
      case _ =>
        Sphere(invalidPoint, 1f)
    }
  }
}
