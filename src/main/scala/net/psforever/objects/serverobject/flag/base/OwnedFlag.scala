// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import net.psforever.objects.PlanetSideGameObject
import net.psforever.objects.serverobject.environment.{EnvironmentAttribute, EnvironmentTrait}
import net.psforever.objects.serverobject.environment.interaction.InteractWithEnvironment
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.objects.serverobject.interior.TraditionalInteriorAware
import net.psforever.objects.serverobject.structures.{Building, WarpGate}
import net.psforever.objects.zones.{Zone, ZoneAware}
import net.psforever.objects.zones.interaction.InteractsWithZone
import net.psforever.services.local.support.{CaptureEnvelope, HackCaptureActor, VanuModuleEnvelope, VanuModuleManager}
import net.psforever.types.{PlanetSideGUID, Vector3}

object OwnedFlag {
  /**
   * na
   * @param flagGuid flag that may exist
   * @param target evaluate this to determine if to continue with this loss
   */
  def ReasonToLoseFlagViolently(
                                 zone: Zone,
                                 flagGuid: Option[PlanetSideGUID],
                                 target: PlanetSideGameObject with InteractsWithZone
                               ): Boolean = {
    zone
      .GUID(flagGuid)
      .collect {
        case flag: VanuModule
          if LoseFlagViolentlyToEnvironment(target, Set(EnvironmentAttribute.Water, EnvironmentAttribute.Lava, EnvironmentAttribute.Death)) =>
          zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.ModuleLost(flag))
          true
        case flag: OwnedFlag
          if flag.ValidFlagType == CarriableFlag.CaptureFlag &&
            LoseFlagViolentlyToEnvironment(target, Set(EnvironmentAttribute.Water, EnvironmentAttribute.Lava, EnvironmentAttribute.Death)) /*||
            LoseFlagViolentlyToWarpGateEnvelope(zone, target)*/ =>
          flag.Destroyed = true
          zone.LocalEvents ! CaptureEnvelope(HackCaptureActor.FlagLost(flag))
          true
      }
      .getOrElse(false)
  }

  def LoseFlagViolentlyToEnvironment(
                                      target: PlanetSideGameObject with InteractsWithZone,
                                      deniedEnvironments: Set[EnvironmentTrait]
                                    ): Boolean = {
    target
      .interaction()
      .collectFirst { case env: InteractWithEnvironment => env.OngoingInteractions }
      .map(_.intersect(deniedEnvironments))
      .getOrElse(Set())
      .nonEmpty
  }

  def LoseFlagViolentlyToWarpGateEnvelope(
                                           zone: Zone,
                                           target: PlanetSideGameObject with InteractsWithZone
                                         ): Boolean = {
    val position = target.Position
    zone
      .blockMap
      .sector(position, range = 10f)
      .buildingList
      .collectFirst {
        case gate: WarpGate if Vector3.DistanceSquared(position, gate.Position) < math.pow(gate.Definition.SOIRadius, 2f) => gate
      }
      .nonEmpty
  }
}

class OwnedFlag(private val fDef: FlagDefinition, validFlagType: CarriableFlag)
  extends Flag(fDef, validFlagType)
    with ZoneAware
    with TraditionalInteriorAware {
  private var owner: Building = Building.NoBuilding
  private var target: Building = Building.NoBuilding

  def Owner: Building = owner

  def Owner_=(obj: Building): Building = {
    owner = obj
    Owner
  }

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

  def isInstalled: Boolean = Carrier.isEmpty && Target != Building.NoBuilding
}
