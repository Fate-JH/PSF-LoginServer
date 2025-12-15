// Copyright (c) 2025 PSForever
package net.psforever.objects.serverobject.dome

import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.serverobject.affinity.{FactionAffinity, FactionAffinityBehavior}
import net.psforever.objects.serverobject.structures.{Amenity, Building, PoweredAmenityControl}
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAware, CaptureTerminalAwareBehavior}
import net.psforever.objects.serverobject.turret.FacilityTurret
import net.psforever.packet.game.ChatMsg
import net.psforever.services.Service
import net.psforever.services.local.{LocalAction, LocalServiceMessage}
import net.psforever.types.{ChatMessageType, PlanetSideEmpire, PlanetSideGeneratorState, Vector3}

object ForceDomeControl {
  trait Command

  final case object CustomExpand extends Command

  final case object CustomCollapse extends Command

  final case object NormalBehavior extends Command

  /**
   * Dispatch a message to update the state of the clients with the server state of the capitol force dome.
   * @param dome force dome
   * @param activationState new force dome status
   */
  def ChangeDomeEnergizedState(dome: ForceDomePhysics, activationState: Boolean): Unit = {
    dome.Energized = activationState
    val owner = dome.Owner
    val zone = owner.Zone
    zone.LocalEvents ! LocalServiceMessage(
      zone.id,
      LocalAction.UpdateForceDomeStatus(Service.defaultPlayerGUID, owner.GUID, activationState)
    )
  }

  /**
   * If this building is a capitol major facility,
   * use the faction affinity, the generator status, and the resource silo's capacitance level
   * to determine if the capitol force dome should be active.
   * @param building building being evaluated
   * @param dome force dome
   * @return the condition of the capitol force dome;
   *         `None`, if the facility is not a capitol building;
   *         `Some(true|false)` to indicate the state of the force dome
   */
  def CheckForceDomeStatus(building: Building, dome: ForceDomePhysics): Option[Boolean] = {
    if (building.IsCapitol) {
      Some(
        if (ForceDomeControl.InvalidBuildingCapitolForceDomeConditions(building)) {
          false
        } else {
          building
            .Neighbours(building.Faction)
            .map(_.count(b => !ForceDomeControl.InvalidBuildingCapitolForceDomeConditions(b)))
            .exists(_ > 1)
        }
      )
    } else {
      None
    }
  }

  /**
   * The natural conditions of a facility that is not eligible for its capitol force dome to be expanded.
   * The only test not employed is whether or not the target building is a capitol.
   * Omission of this condition makes this test capable of evaluating subcapitol eligibility
   * for capitol force dome expansion.
   * @param building target building
   * @return `true`, if the conditions for capitol force dome are not met;
   *        `false`, otherwise
   */
  def InvalidBuildingCapitolForceDomeConditions(building: Building): Boolean = {
    building.Faction == PlanetSideEmpire.NEUTRAL ||
      building.NtuLevel == 0 ||
      building.Generator.exists(_.Condition == PlanetSideGeneratorState.Destroyed)
  }

  /**
   * na
   * @param dome force dome
   * @return na
   */
  def GeneralFacilityPerimeter(dome: ForceDomePhysics): List[(Vector3, Vector3)] = {
    val generatorTowerCenter = dome.Position.xy
    val turretPoints = dome.Owner.Amenities.filter(_.isInstanceOf[FacilityTurret]).map(_.Position.xy)
    val pointsOfForceDomePerimeter = turretPoints.map { p =>
      val segmentFromTowerToTurret = p - generatorTowerCenter
      Vector3.Unit(segmentFromTowerToTurret) * (Vector3.Magnitude(segmentFromTowerToTurret) + 20) //todo get correct distance offset
    }
    pointsOfForceDomePerimeter
      .flatMap { point =>
        pointsOfForceDomePerimeter
          .sortBy(p => Vector3.DistanceSquared(p, point))
          .slice(1, 3)
          .map { otherPoint =>
            if (point.y > otherPoint.y || point.x < otherPoint.x) {
              (point, otherPoint)
            } else {
              (otherPoint, point)
            }
          }
      }
      .distinct
  }

  def TechPlantFacilityPerimeter(dome: ForceDomePhysics): List[(Vector3, Vector3)] = {
    val generatorTowerCenter = dome.Position.xy
    val turretPoints = dome.Owner.Amenities.filter(_.isInstanceOf[FacilityTurret]).map(_.Position.xy)
    val organizedByClosestToGarage = dome
      .Owner
      .Amenities
      .find(_.Definition.Name.equals("gr_door_garage_ext"))
      .map { garage =>
        val doorPosition = garage.Position.xy
        turretPoints.sortBy(point => Vector3.DistanceSquared(doorPosition, point))
      }
      .getOrElse(List[Vector3]())

    //val turretPoints = dome.Owner.Amenities.filter(_.isInstanceOf[FacilityTurret]).map(_.Position.xy)
    val pointsOfForceDomePerimeter = turretPoints.map { p =>
      val segmentFromTowerToTurret = p - generatorTowerCenter
      Vector3.Unit(segmentFromTowerToTurret) * (Vector3.Magnitude(segmentFromTowerToTurret) + 20) //todo get correct distance offset
    }
    Nil
  }

  /**
   * na
   * @param building target building
   * @param state na
   */
  def CustomDomeStateEnforcedMessage(
                                      building: Building,
                                      state: Boolean
                                    ): Unit = {
    val events = building.Zone.LocalEvents
    val message = LocalAction.SendResponse(ChatMsg(
      ChatMessageType.UNK_227,
      s"Capitol force dome state change was suppressed.  ${building.Name} will remain ${if (state) "enveloped" else "exposed"}."
    ))
    building.PlayersInSOI.foreach { player =>
      events ! LocalServiceMessage(player.Name, message)
    }
  }

  /**
   * na
   * @param building target building
   */
  def NormalDomeStateMessage(building: Building): Unit = {
    val events = building.Zone.LocalEvents
    val message = LocalAction.SendResponse(ChatMsg(
      ChatMessageType.UNK_227,
      "Expected capitol force dome state change will resume."
    ))
    building.PlayersInSOI.foreach { player =>
      events ! LocalServiceMessage(player.Name, message)
    }
  }
}

/**
 * An `Actor` that handles messages being dispatched to a specific capitol facility's force dome.
 * @param dome the `ForceDomePhysics` object being governed
 */
class ForceDomeControl(dome: ForceDomePhysics)
  extends PoweredAmenityControl
    with CaptureTerminalAwareBehavior
    with FactionAffinityBehavior.Check {
  def CaptureTerminalAwareObject: Amenity with CaptureTerminalAware = dome
  def FactionObject: FactionAffinity = dome

  private var perimeterSegments: List[(Vector3, Vector3)] = Nil

  private lazy val domeOwnerAsABuilding = dome.Owner.asInstanceOf[Building]

  private var customState: Option[Boolean] = None

  def commonBehavior: Receive = checkBehavior
    .orElse {
      case Service.Startup() =>
        setupPerimeter()

      case ForceDomeControl.CustomExpand
        if !dome.Energized && (customState.isEmpty || customState.contains(false)) =>
        customState = Some(true)
        ForceDomeControl.CustomDomeStateEnforcedMessage(domeOwnerAsABuilding, state = true)
        ForceDomeControl.ChangeDomeEnergizedState(dome, activationState = true)

      case ForceDomeControl.CustomExpand
        if customState.isEmpty =>
        customState = Some(true)
        ForceDomeControl.CustomDomeStateEnforcedMessage(domeOwnerAsABuilding, state = true)

      case ForceDomeControl.CustomCollapse
        if dome.Energized && (customState.isEmpty || customState.contains(true)) =>
        customState = Some(false)
        ForceDomeControl.CustomDomeStateEnforcedMessage(domeOwnerAsABuilding, state = false)
        ForceDomeControl.ChangeDomeEnergizedState(dome, activationState = false)

      case ForceDomeControl.CustomCollapse
        if customState.isEmpty =>
        customState = Some(false)
        ForceDomeControl.CustomDomeStateEnforcedMessage(domeOwnerAsABuilding, state = false)

      case ForceDomeControl.NormalBehavior
        if customState.nonEmpty =>
        customState = None
        ForceDomeControl.NormalDomeStateMessage(domeOwnerAsABuilding)
        alignForceDomeStatusAndUpdate(domeOwnerAsABuilding)
    }

  def poweredStateLogic: Receive = {
    commonBehavior
      .orElse(captureTerminalAwareBehaviour)
      .orElse {
        case BuildingActor.AlertToFactionChange(_) =>
          blockedByCustomStateOr(alignForceDomeStatusAndUpdate, domeOwnerAsABuilding)

        case _ => ()
      }
  }

  def unpoweredStateLogic: Receive = {
    commonBehavior
      .orElse {
        case _ => ()
      }
  }

  def powerTurnOffCallback() : Unit = {
    if (dome.Energized && customState.isEmpty) {
      ForceDomeControl.ChangeDomeEnergizedState(dome,activationState = false)
    }
  }

  def powerTurnOnCallback() : Unit = {
    blockedByCustomStateOr(alignForceDomeStatus, domeOwnerAsABuilding)
  }

  override protected def captureTerminalIsResecured(terminal: CaptureTerminal): Unit = {
    super.captureTerminalIsResecured(terminal)
    blockedByCustomStateOr(alignForceDomeStatus, domeOwnerAsABuilding)
  }

  override protected def captureTerminalIsHacked(terminal: CaptureTerminal): Unit = {
    super.captureTerminalIsHacked(terminal)
    if (dome.Energized && customState.isEmpty) {
      ForceDomeControl.ChangeDomeEnergizedState(dome, activationState = false)
    }
  }

  private def setupPerimeter(): Unit = {
    //todo tech plants have an indent
    if (perimeterSegments.isEmpty) {
      perimeterSegments = ForceDomeControl.GeneralFacilityPerimeter(dome)
    }
  }

  /**
   * na
   * @param func function to run if not blocked
   * @param building facility to operate upon (parameter to `func`)
   * @return next behavior for an actor state
   */
  private def blockedByCustomStateOr(func: Building => Unit, building: Building): Unit = {
    customState match {
      case None =>
        func(building)
      case Some(state) =>
        ForceDomeControl.CustomDomeStateEnforcedMessage(building, state)
    }
  }

  /**
   * Evaluate the conditions of the building
   * and determine if its capitol force dome state should be updated
   * to reflect the actual conditions of the base or its surrounding bases.
   * If this building is considered a subcapitol facility to the zone's actual capitol facility,
   * and has the capitol force dome has a dependency upon it,
   * pass a message onto that facility that it should check its own state alignment.
   * @param building the building being evaluated
   */
  private def alignForceDomeStatusAndUpdate(building: Building): Unit = {
    ForceDomeControl.CheckForceDomeStatus(building, dome).foreach {
      updatedStatus =>
        if (updatedStatus != dome.Energized) {
          ForceDomeControl.ChangeDomeEnergizedState(dome, updatedStatus)
          dome.Owner.Actor ! BuildingActor.MapUpdate()
        }
    }
  }

  /**
   * Evaluate the conditions of the building
   * and determine if its capitol force dome state should be updated
   * to reflect the actual conditions of the base or its surrounding bases.
   * If this building is considered a subcapitol facility to the zone's actual capitol facility,
   * and has the capitol force dome has a dependency upon it,
   * pass a message onto that facility that it should check its own state alignment.
   * @param building the building being evaluated
   */
  private def alignForceDomeStatus(building: Building): Unit = {
    ForceDomeControl.CheckForceDomeStatus(building, dome).foreach {
      updatedStatus =>
        if (updatedStatus != dome.Energized) {
          ForceDomeControl.ChangeDomeEnergizedState(dome, updatedStatus)
        }
    }
  }
}
