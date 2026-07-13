package net.psforever.objects.serverobject.flag.base

import net.psforever.objects.serverobject.structures.AmenityDefinition

class FlagSocketDefinition(private val objectId: Int, val ValidFlagType: FlagType)
  extends AmenityDefinition(objectId)
    with DesignatedFlagType {
  Name = "flag_socket"
}
