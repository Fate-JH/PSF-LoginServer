// Copyright (c) 2025 PSForever
package net.psforever.objects.serverobject.dome

import net.psforever.objects.serverobject.structures.AmenityDefinition
import net.psforever.types.Vector3

class ForceDomeDefinition(objectId: Int)
  extends AmenityDefinition(objectId) {
  Name = "force_dome"
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
