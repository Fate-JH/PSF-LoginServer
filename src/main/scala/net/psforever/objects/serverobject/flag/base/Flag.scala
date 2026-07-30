// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import net.psforever.objects.entity.WorldEntity
import net.psforever.objects.serverobject.affinity.FactionAffinity
import net.psforever.objects.{PlanetSideGameObject, Player}
import net.psforever.objects.serverobject.structures.{Amenity, AmenityDefinition, Building}
import net.psforever.types.{PlanetSideEmpire, Vector3}

/**
 * Represent a special entity that is carried by the player in certain circumstances.
 * The entity is not a piece of `Equipment` so it does not go into the holsters,
 * does not into the player's inventory,
 * and is not carried in or manipulated by the player's hands.
 * The different game elements it simulates are:
 * a facility's lattice logic unit (LLU),
 * the cavern modules,
 * and the rabbit ball (special game mode).
 */
trait IsAFlag
    extends DesignatedFlagType
      with WorldEntity
      with FactionAffinity {
  def Faction: PlanetSideEmpire.Value
  def Faction_=(newFaction: PlanetSideEmpire.Value): PlanetSideEmpire.Value

  def Carrier: Option[Player]
  def Carrier_=(newCarrier: Option[Player]) : Option[Player]

  def LastCollectionTime: Long

  def InitialSpawnTime: Long
}

class Flag(private val fDef: FlagDefinition, val ValidFlagType: FlagType)
    extends PlanetSideGameObject
      with IsAFlag {
  private var faction: PlanetSideEmpire.Value = PlanetSideEmpire.NEUTRAL
  private var carrier: Option[Player] = None
  private var lastTimeCollected: Long = System.currentTimeMillis()
  private val spawnedTime: Long = lastTimeCollected

  /**
   * Flags are primarily neutral and act as if aligned with the faction of the player that holds them.
   */
  override def Faction: PlanetSideEmpire.Value = faction
  override def Faction_=(newFaction: PlanetSideEmpire.Value): PlanetSideEmpire.Value = {
    faction = newFaction
    faction
  }

  /**
   * When the flag is carried by a player, the position returned should be that of the carrier not the flag.
   * @return the position of the carrier, if there is a player carrying the flag, or the flag itself
   */
  override def Position: Vector3 = {
    carrier match {
      case Some(player) => player.Position
      case None => super.Position
    }
  }

  def Carrier: Option[Player] = carrier
  def Carrier_=(newCarrier: Option[Player]) : Option[Player] = {
    carrier = newCarrier
    lastTimeCollected = System.currentTimeMillis()
    carrier
  }

  def LastCollectionTime: Long = carrier.map { _ => lastTimeCollected }.getOrElse { System.currentTimeMillis() }

  def InitialSpawnTime: Long = spawnedTime

  def Definition: FlagDefinition = fDef
}

class OwnedFlag(private val fDef: FlagDefinition, val ValidFlagType: FlagType)
  extends Amenity
    with IsAFlag {
  private val flag: Flag = new Flag(fDef, ValidFlagType)
  private var target: Building = Building.NoBuilding

  /**
   * If a facility spawns a flag, that is the flag's `owner`.
   * The target facility is the expectorant destination of the flag while it is been carried or
   * the current installed facility while it is away from it origin (`owner`) facility.
   */
  def Target: Building = target
  def Target_=(newTarget: Building): Building = {
    target = newTarget
    target
  }

  override def Faction_=(newFaction: PlanetSideEmpire.Value): PlanetSideEmpire.Value = {
    flag.Faction_=(newFaction)
    super.Faction_=(newFaction)
  }

  override def Position: Vector3 = flag.Position
  override def Position_=(vec: Vector3): Vector3 = {
    flag.Position_=(vec)
    super.Position_=(vec)
  }

  def Carrier: Option[Player] = flag.Carrier
  def Carrier_=(newCarrier: Option[Player]) : Option[Player] = flag.Carrier_=(newCarrier)

  def LastCollectionTime: Long = flag.LastCollectionTime

  def InitialSpawnTime: Long = flag.InitialSpawnTime

  def Definition: AmenityDefinition = fDef
}
