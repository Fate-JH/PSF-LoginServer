package net.psforever.actors.zone.building
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.ActorContext
import net.psforever.actors.commands.NtuCommand
import net.psforever.actors.zone.{BuildingActor, BuildingControlDetails}
import net.psforever.objects.serverobject.structures.{Amenity, Building}
import net.psforever.types.PlanetSideEmpire

object CavernModuleFacilityLogic extends BuildingLogic {
  override def wrapper(
                        building: Building,
                        context: ActorContext[BuildingActor.Command],
                        details: BuildingControlDetails
                      ): BuildingWrapper = {
    //todo wrapper only fires when building starts; send one-way message to vanu module spawner service
    FacilityWrapper(building, context, details.galaxyService, details.interstellarCluster)
  }

  def amenityStateChange(details: BuildingWrapper, entity: Amenity, data: Option[Any]): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.amenityStateChange(details, entity, data)

  def powerOff(details: BuildingWrapper): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.powerOff(details)

  def powerOn(details: BuildingWrapper): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.powerOn(details)

  def ntuDepleted(details: BuildingWrapper): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.ntuDepleted(details)

  def suppliedWithNtu(details: BuildingWrapper): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.suppliedWithNtu(details)

  /**
   * The facility will change its faction affiliation.
   * Update all related subsystems and statuses.
   *
   * @param details package class that conveys the important information
   * @param faction the faction affiliation to which the facility will update
   * @return the next behavior for this control agency messaging system
   */
  def setFactionTo(details: BuildingWrapper, faction: PlanetSideEmpire.Value): Behavior[BuildingActor.Command] = {
    val beforeFaction = details.building.Faction
    val out = CavernFacilityLogic.setFactionTo(details, faction)
    if (beforeFaction == PlanetSideEmpire.NEUTRAL) {
      //todo alert module spawn control
    }
    out
  }

  def alertToFactionChange(details: BuildingWrapper, building: Building): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.alertToFactionChange(details, building)

  def ntu(details: BuildingWrapper, msg: NtuCommand.Command): Behavior[BuildingActor.Command] =
    CavernFacilityLogic.ntu(details, msg)
}
