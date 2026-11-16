// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import net.psforever.objects.entity.WorldEntity
import net.psforever.objects.serverobject.affinity.FactionAffinity
import net.psforever.objects.{PlanetSideGameObject, Player}
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

class Flag(fDef: FlagDefinition, validFlagType: CarriableFlag)
    extends PlanetSideGameObject
      with FactionAffinity
      with IsAFlag {
  private var faction: PlanetSideEmpire.Value = PlanetSideEmpire.NEUTRAL
  private var carrier: Option[Player] = None
  private var lastTimeCollected: Long = System.currentTimeMillis()
  private val spawnedTime: Long = lastTimeCollected

  def ValidFlagType: CarriableFlag = validFlagType

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
