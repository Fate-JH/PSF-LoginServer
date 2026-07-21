// Copyright (c) 2021 PSForever
package net.psforever.objects.serverobject.flag.llu

import net.psforever.objects.serverobject.flag.base.{FlagDefinition, FlagType, OwnedFlag}
import net.psforever.objects.serverobject.structures.{AmenityOwner, Building}
import net.psforever.objects.GlobalDefinitions
import net.psforever.types.{PlanetSideEmpire, Vector3}

/**
 * Represent a special entity that is carried by the player in certain circumstances.<br>
 * <br>
 * For the lattice logic unit, when a facility is set to generate an LLU upon hack,
 * and an adjacent facility on the lattice provides an accommodating faction connection,
 * the unit gets spawned at the LLU socket within the hacked facility.
 * The LLU socket actually doesn't do anything but keep track of the spawned flag and provide a location.
 * It associates with the faction of the hacker and, carried by other players of the same faction only,
 * must be brought to the control console of a designated facility that is owned by the faction of the hacking empire.
 * If the hack is cancelled through a resecure, the LLU despawns.
 * If the facility is counter-hacked, the active LLU despawns and a new LLU is spawned in the socket.
 * Other empires can not interact with the LLU while it is dropped on the ground and
 * vehicles will be warned and then deconstructed if they linges too long near a dropped LLU.
 * The LLU can not be submerged in water or it will despawn and the hack will cancel.
 */
object CaptureFlag {
  def apply(tDef: FlagDefinition): OwnedFlag = {
    new OwnedFlag(tDef, FlagType.CaptureFlag)
  }

  def Constructor(pos: Vector3, ori: Vector3, target: Building, owner: AmenityOwner, faction: PlanetSideEmpire.Value) : OwnedFlag = {
    val obj = CaptureFlag(GlobalDefinitions.capture_flag)
    obj.Position = pos
    obj.Orientation = ori
    obj.Target = target
    obj.Owner = owner
    obj.Faction = faction
    obj
  }
}
