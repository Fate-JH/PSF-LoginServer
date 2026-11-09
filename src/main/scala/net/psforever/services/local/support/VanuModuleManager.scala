// Copyright (c) 2026 PSForever
package net.psforever.services.local.support

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorContext, ActorRef, Cancellable, Props}
import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.{Default, GlobalDefinitions, Player}
import net.psforever.objects.definition.converter.OCM
import net.psforever.objects.guid.{GUIDTask, StraightforwardTask, TaskBundle, TaskWorkflow}
import net.psforever.objects.serverobject.beam.VanuModuleBeam
import net.psforever.objects.serverobject.damage.Damageable
import net.psforever.objects.serverobject.flag.base.{CarriableFlag, OwnedFlag}
import net.psforever.objects.serverobject.flag.module.{ModuleChargeState, VanuModule, VanuModuleNode}
import net.psforever.objects.serverobject.structures.Building
import net.psforever.objects.zones.Zone
import net.psforever.packet.game.packets.{GenericObjectActionEnum, ObjectAttachMessage, ObjectDetachMessage, TriggeredSound}
import net.psforever.services.ServiceManager.{Lookup, LookupResult}
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.{GenericObjectAction, ObjectDelete, PlanetsideAttribute, SelfRespondingEvent, SendResponse}
import net.psforever.services.{ServiceManager, VanuModuleRotationService}
import net.psforever.services.base.{EventServiceSupport, GenericSupportEnvelopeOnly}
import net.psforever.services.local.LocalAction
import net.psforever.types.{PlanetSideEmpire, PlanetSideGUID, Vector3}

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

case class VanuModuleSupport(zone: Zone)
  extends EventServiceSupport {
  def label: String = "vanuModuleManager"
  def constructor(context: ActorContext): ActorRef = {
    context.actorOf(Props(classOf[VanuModuleManager], zone), name = "VanuModuleManager")
  }
}

final case class VanuModuleEnvelope(supportMessage: VanuModuleManager.Command)
  extends GenericSupportEnvelopeOnly {
  def supportLabel: String = "vanuModuleManager"
}

final case class VanuModuleUpdate(
                                   toZone: Int,
                                   modules: Seq[(CarriableFlag, PlanetSideEmpire.Value, Long, Vector3)]
                                 ) extends SelfRespondingEvent

object VanuModuleManager {
  final val FlagHoverHeight: Vector3 = Vector3.z(value = 1.05f)

  final val GlobalModuleOwnershipCount: scala.collection.concurrent.TrieMap[PlanetSideEmpire.Value, Int] =
    scala.collection.concurrent.TrieMap.from(PlanetSideEmpire.values.map((_, 0)))

  trait Command

  final case class ModuleSpawnerReady(obj: VanuModuleNode) extends Command

  final case class ModuleSpawnerUnready(obj: VanuModuleNode) extends Command

  final case class NewModuleInZone(module: VanuModule, player: Player) extends Command

  final case class TrackModuleInZone(module: VanuModule) extends Command

  final case class StopTrackingModule(module: VanuModule) extends Command

  final case class ModuleLimitReached(module: VanuModule)

  final case class WhichModulesToDisplay(channel: String) extends Command

  final case class DropModule(flag: OwnedFlag) extends Command

  final case class PickupModuleFromCradle(spawner: VanuModuleNode, player: Player) extends Command

  final case class PickUpModule(flag: VanuModule, player: Player) extends Command

  final case class StartChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class StopChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeComplete(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeModule(module: VanuModule) extends Command

  final case class ModuleLost(module: VanuModule) extends Command

  final case class ModuleFacilityLost(facility: Building) extends Command

  final case class InstallModuleIntoCradle(cradle: VanuModuleNode, module: VanuModule, player: Player) extends Command

  final case class FreeModuleFromCradle(cradle: VanuModuleNode) extends Command

  private case object DisplayVanuModuleIconsInZone extends Command

  def CradleReadyToFunctionStatus(cradle: VanuModuleNode): Boolean = {
    cradle.Faction != PlanetSideEmpire.NEUTRAL &&
      cradle.Owner.asInstanceOf[Building].CaptureTerminal.forall(_.HackedBy.isEmpty)
  }

  private def IsFunctionalModule(module: VanuModule): Boolean = {
    module.GetRemainingTimeAlive() > 0L && module.GetRemainingExpirationTime() > 0L
  }

  def FactionCanOwnMoreModules(faction: PlanetSideEmpire.Value, zone: Zone): Boolean = {
    GlobalModuleOwnershipCount(faction) < zone.Info.moduleRoamingLimit
  }

  def NumberOfModulesFactionOwns(faction: PlanetSideEmpire.Value): Int = {
    GlobalModuleOwnershipCount(faction)
  }

  private def TryGiveFactionModuleOwnership(
                                             faction: PlanetSideEmpire.Value,
                                             moduleFaction: PlanetSideEmpire.Value,
                                             zone: Zone
                                           ): Boolean = {
    if (FactionCanOwnMoreModules(faction, zone) || faction != moduleFaction) {
      GlobalModuleOwnershipCount.update(faction, GlobalModuleOwnershipCount(faction) + 1)
      true
    } else {
      false
    }
  }

  private def RemoveModuleFromFactionOwnership(faction: PlanetSideEmpire.Value): Unit = {
    val updatedCount = math.max(GlobalModuleOwnershipCount(faction) - 1, 0)
    GlobalModuleOwnershipCount.update(faction, updatedCount)
  }
}

class VanuModuleManager(zone: Zone) extends Actor {
  import VanuModuleManager._

  private var cradles: Seq[VanuModuleNode] = Seq()
  private var trackedModules: Seq[VanuModule] = Seq()

  private var vanuModuleUpdateTimer: Cancellable = Default.Cancellable
  private var galaxyService: ActorRef = Default.Actor

  override def preStart(): Unit = {
    super.preStart()
    ServiceManager.serviceManager ! Lookup("galaxy")
  }

  val AddRemoveCradles: Receive = {
    case ModuleSpawnerReady(spawner) =>
      addSpawner(spawner)

    case ModuleSpawnerUnready(spawner) =>
      removeSpawner(spawner)
  }

  def receive: Receive = setupBehaviorGalaxy

  private val setupBehaviorGalaxy: Receive = AddRemoveCradles.orElse {
    case LookupResult("galaxy", endpoint) =>
      galaxyService = endpoint
      context.become(setupBehaviorModuleRotation)
      ServiceManager.receptionist ! Receptionist.Find(
        VanuModuleRotationService.VanuModuleRotationServiceKey,
        context.self.toTyped
      )

    case _ => ()
  }

  private val setupBehaviorModuleRotation: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.VanuModuleRotationServiceKey.Listing(listings) =>
      val manager = listings.head
      manager ! VanuModuleRotationService.KeepUpdated(self)
      context.become(setInitialSpawners)

    case _ => ()
  }

  private val setInitialSpawners: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.CurrentModulesAvailable(currentlyAvailableModules) =>
      cradles
        .filter(o => currentlyAvailableModules.contains(o.ValidFlagType))
        .foreach(trySpawnModuleInCradle)
      context.become(standardRotation)

    case _ => ()
  }

  private val standardRotation: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.RotationHasOccurred(previousModules, currentlyAvailableModules) =>
      rotationHasOccurred(previousModules, currentlyAvailableModules)

    case NewModuleInZone(module, host) =>
      newModuleInZone(module, host)

    case TrackModuleInZone(module) =>
      trackModule(module)

    case StopTrackingModule(module) =>
      untrackModule(module)

    case WhichModulesToDisplay(channel) =>
      whichModulesToDisplay(channel)

    case VanuModuleManager.DisplayVanuModuleIconsInZone =>
      displayVanuModuleIconsInZone()

    case PickupModuleFromCradle(spawner, player) =>
      pickupModuleFromCradle(spawner, player)

    case DropModule(flag) =>
      dropModule(flag)

    case PickUpModule(flag, player) =>
      pickUpModule(flag, player)

    case StartChargingModule(module, beam, player) =>
      startChargingModule(module, beam, player)

    case StopChargingModule(module, beam, player) =>
      stopChargingModule(module, beam, player)

    case ChargeComplete(module, beam, player) =>
      chargeComplete(module, beam, player)

    case ChargeModule(flag)
      if flag.Charged == ModuleChargeState.Charging =>
      chargeModule(flag)

    case ModuleLost(flag) =>
      moduleLost(flag)

    case ModuleFacilityLost(facility) =>
      moduleFacilityLost(facility)

    case InstallModuleIntoCradle(cradle, flag, player) =>
      installModuleIntoCradle(cradle, flag, player)

    case FreeModuleFromCradle(cradle) =>
      freeModuleFromCradle(cradle)

    case _ => ()
  }

  private def addSpawner(spawner: VanuModuleNode): Unit = {
    val spawnerGuid = spawner.GUID
    cradles
      .find(_.GUID == spawnerGuid)
      .getOrElse {
        cradles = cradles :+ spawner
        spawner.captureFlag.collect { case o: VanuModule => trackModule(o) }
      }
  }

  private def removeSpawner(spawner: VanuModuleNode): Unit = {
    val spawnerGuid = spawner.GUID
    cradles.indexWhere(_.GUID == spawnerGuid) match {
      case -1 => ()
      case index =>
        cradles = cradles.take(index) ++ cradles.drop(index + 1)
        spawner.captureFlag.collect { case o: VanuModule => untrackModule(o) }
    }
  }

  private def rotationHasOccurred(previousModules: Seq[CarriableFlag], currentlyAvailableModules: Seq[CarriableFlag]): Unit = {
    val previousModuleSet = previousModules.toSet
    val modulesToActivate = currentlyAvailableModules.toSet.diff(previousModuleSet)
    val modulesToClear = previousModuleSet.diff(modulesToActivate)
    cradles
      .filter(o => modulesToClear.contains(o.ValidFlagType))
      .foreach(tryDespawnModuleAssociatedWithCradle)
    cradles
      .filter(o => modulesToActivate.contains(o.ValidFlagType))
      .foreach(trySpawnModuleInCradle)
  }

  private def whichModulesToDisplay(channel: String): Unit = {
    val outputList = trackedModules
      .flatMap { flag =>
        val objectCreatePacket = OCM.apply(flag)
        flag.Carrier
          .map(p => ObjectAttachMessage(p.GUID, flag.GUID, Player.SpecialCarrySlot))
          .orElse {
            val results = if (flag.Target == flag.Owner) {
              VanuModuleNode.ThatSpawnedThis(flag)
            } else {
              VanuModuleNode.ThatSpawnedThis(flag, flag.Target)
            }
            if (results.hasModuleInstalled) {
              results.GetSpawner.map(o => ObjectAttachMessage(o.GUID, flag.GUID, 0))
            } else {
              None
            }
          }
          .map(attachPacket => List(objectCreatePacket, attachPacket))
          .getOrElse(List(objectCreatePacket))
      }
    if (outputList.nonEmpty) {
      zone.LocalEvents ! MessageEnvelope(channel, SendResponse(outputList))
    }
  }

  private def displayVanuModuleIconsInZone(): Unit = {
    retireExpiredModules()
    updateVanuModuleIcons()
    if (trackedModules.isEmpty) {
      vanuModuleUpdateTimer.cancel()
      vanuModuleUpdateTimer = Default.Cancellable
    }
  }

  private def retireExpiredModules(): Unit = {
    val (modulesToContinue, modulesToRetire) = trackedModules.partition(IsFunctionalModule)
    trackedModules = modulesToContinue
    modulesToRetire.foreach { module =>
      val cradleFromModule = VanuModuleNode.ThatContainsThis(module)
      if (cradleFromModule.hasModuleInstalled) {
        cradleFromModule.GetSpawner.foreach(tryDespawnModuleAssociatedWithCradle(_, module))
      } else {
        handleFlagDespawnIfCarried(module)
      }
    }
  }

  private def updateVanuModuleIcons(): Unit = {
    val outputList = trackedModules
      .map { module => (module.DisplayFlagType, module.Faction, 0L, module.Position.xy) }
    galaxyService ! MessageEnvelope("", VanuModuleUpdate(zone.Number, outputList))
  }

  private def pickupModuleFromCradle(cradle: VanuModuleNode, player: Player): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) if canHaveMoreRoamingModulesInZone =>
        val zoneid = flag.Zone.id
        val flagGuid = flag.GUID
        val playerPosition = player.Position
        val flagCategory = flag.ValidFlagType.category
        flag.Carrier = Some(player)
        player.Carrying = Some(flagCategory)
        flag.Target = Building.NoBuilding
        if (flag.Charged != ModuleChargeState.Charged) {
          flag.Charged = false //if not explicitly charged, set to uncharged (false)
        }
        flag.Position = Vector3.Zero //spawn far out of sight, at first
        flag.SetTime(durationTime = 900000L) //(ms; or, 15 minutes)
        val flagOcmPacket = OCM.apply(flag)
        flag.Position = playerPosition
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, SendResponse(
            ObjectDetachMessage(cradle.GUID, flagGuid, cradle.Position, 0, 0, 0), /* cradle emits module */
            flagOcmPacket, /* overwrite with concealed module */
            ObjectAttachMessage(player.GUID, flagGuid, Player.SpecialCarrySlot) /* module held by player */
          )),
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, playerPosition, 15, volume = 0.8f)),
          MessageEnvelope(player.Name, LocalAction.PlayerCollectsFlag(flagCategory, flag.GUID))
        )
      case _ => ()
    }
  }

  private def dropModule(flag: OwnedFlag): Unit = {
    flag.Carrier match {
      case Some(player: Player) =>
        val zoneid = zone.id
        val playerPosition = player.Position
        val flagPosition = playerPosition + FlagHoverHeight
        val flagOrientation = player.Orientation
        flag.Position = flagPosition
        flag.Orientation = flagOrientation
        flag.Carrier = None
        player.Carrying = None
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, playerPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(player.GUID, flag.GUID, flagPosition, 0, 0, flagOrientation.z))),
          MessageEnvelope(player.Name, LocalAction.PlayerDropsFlag(flag.GUID))
        )
      case _ => ()
    }
  }

  private def pickUpModule(flag: VanuModule, player: Player): Unit = {
    lazy val isFunctional = IsFunctionalModule(flag)
    flag.Carrier match {
      case None if isFunctional =>
        val zoneid = flag.Zone.id
        val flagCategory = flag.ValidFlagType.category
        flag.Carrier = Some(player)
        player.Carrying = Some(flagCategory)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, player.Position, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(player.GUID, flag.GUID, Player.SpecialCarrySlot))),
          MessageEnvelope(player.Name, LocalAction.PlayerCollectsFlag(flagCategory, flag.GUID))
        )
      case None if !isFunctional =>
        flag.Destroyed = true
        moduleLost(flag)
      case _ => ()
    }
  }

  private def startChargingModule(module: VanuModule, @unused beam: VanuModuleBeam, player: Player): Unit = {
    zone.LocalEvents ! MessageEnvelope(zone.id, PlanetsideAttribute(player.GUID, 73, 1)) //charging pose
    //todo progress event @VanuModuleChargeProgress?
    //todo event banner @ChargingVanuModuleMessage?
    module.Charged = ModuleChargeState.Charging
    player.Actor ! Damageable.MakeInvulnerable
  }

  private def stopChargingModule(module: VanuModule, @unused beam: VanuModuleBeam, player: Player): Unit = {
    zone.LocalEvents ! MessageEnvelope(zone.id, PlanetsideAttribute(player.GUID, 73, 0)) //undo charging pose
    module.Charged = ModuleChargeState.NonPowered
    player.Actor ! Damageable.MakeVulnerable
  }

  private def chargeComplete(module: VanuModule, beam: VanuModuleBeam, player: Player): Unit = {
    val zoneid = zone.id
    stopChargingModule(module, beam, player)
    module.Charged = true
    module.SetTime(durationTime = 900000L) //(ms; or, 15 minutes)
    VanuModuleNode.ThatSpawnedThis(module).ClearModule()
    val playerPosition = player.Position
    module.Position = Vector3.Zero //spawn far out of sight
    val flagOcmPacket = OCM.apply(module)
    module.Position = playerPosition
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, playerPosition, 15, volume = 0.8f)),
      MessageEnvelope(zoneid, SendResponse(
        flagOcmPacket, /* overwrite with charged module */
        ObjectAttachMessage(player.GUID, module.GUID, Player.SpecialCarrySlot) /* charged module held by player */
      ))
    )
  }

  private def chargeModule(flag: VanuModule): Unit = {
    val zoneid = zone.id
    flag.Charged = true
    VanuModuleNode.ThatSpawnedThis(flag).ClearModule()
    flag.Carrier match {
      case Some(player) =>
        val playerPosition = player.Position
        flag.Position = Vector3.Zero //spawn far out of sight
        val flagOcmPacket = OCM.apply(flag)
        flag.Position = playerPosition
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, playerPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(
            flagOcmPacket, /* overwrite with charged module */
            ObjectAttachMessage(player.GUID, flag.GUID, Player.SpecialCarrySlot) /* charged module held by player */
          ))
        )
        //todo undo charge pose
        player.Actor ! Damageable.MakeVulnerable
      case None =>
        //we would not normally charge flags that are not being carried, but ...
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, flag.Position, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(OCM.apply(flag))) /* overwrite with charged module */
        )
    }
  }

  private def moduleLost(module: VanuModule): Unit = {
    VanuModuleNode
      .ThatContainsThis(module)
      .GetSpawner
      .map { cradle =>
        if (cradle.Owner eq module.Target) {
          RemoveModuleFromFactionOwnership(cradle.Faction)
        }
        cradle
      }
      .map(tryDespawnModuleAssociatedWithCradle(_, module))
      .getOrElse(handleFlagDespawnIfCarried(module))
  }

  private def moduleFacilityLost(facility: Building): Unit = {
    val (compromisedModules, retainedModules) = trackedModules.partition { module =>
      module.Owner == facility && module.FlagFaction != facility.Faction
    }
    trackedModules = retainedModules
    compromisedModules.foreach { module =>
      module.Destroyed = true
      moduleLost(module)
    }
  }

  private def installModuleIntoCradle(cradle: VanuModuleNode, flag: VanuModule, player: Player): Unit = {
    if (IsFunctionalModule(flag) && TryGiveFactionModuleOwnership(cradle.Faction, flag.FlagFaction, zone)) {
      cradle.captureFlag.getOrElse {
        val zone = cradle.Zone
        val owner = cradle.Owner.asInstanceOf[Building]
        val flagPosition = cradle.Position + FlagHoverHeight
        val flagOrientation = cradle.Orientation
        if (!flag.isExpirationDurationSet) {
          //only set the first installation, lasts until end of module's life
          flag.SetExpirationTime(durationTime = 100800000L) //(ms; or, 28 hours)
        }
        flag.SetTime(Long.MaxValue) //deactivates
        flag.Position = flagPosition
        flag.Orientation = flagOrientation
        flag.Target = owner
        flag.Faction = cradle.Faction
        flag.Carrier = None
        player.Carrying = None
        cradle.captureFlag = flag
        cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(cradle.GUID, flag.GUID, 0))),
          MessageEnvelope(player.Name, LocalAction.PlayerDropsFlag(flag.GUID))
        )
      }
    } else {
      moduleLost(flag)
    }
  }

  private def freeModuleFromCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) =>
        val zone = cradle.Zone
        val zoneid = zone.id
        val flagPosition = cradle.Position + FlagHoverHeight
        val flagOrientation = cradle.Orientation
        RemoveModuleFromFactionOwnership(cradle.Faction)
        cradle.captureFlag = None
        flag.Target = Building.NoBuilding
        flag.Position = flagPosition
        flag.Orientation = flagOrientation
        flag.SetTime(durationTime = 900000L) //(ms; or, 15 minutes)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, flagPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(cradle.GUID, flag.GUID, flagPosition, 0, 0, flagOrientation.z)))
        )
        cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
      case _ => ()
    }
  }

  private def trySpawnModuleInCradle(spawner: VanuModuleNode): Unit = {
    spawner.captureFlag match {
      case None if CradleReadyToFunctionStatus(spawner) =>
        // Construct new flag
        val zone = spawner.Zone
        val flag = new VanuModule(GlobalDefinitions.vanu_module, spawner.ValidFlagType)
        val position = spawner.Position + FlagHoverHeight
        val orientation = spawner.Orientation
        flag.Zone = zone
        flag.Position = position
        flag.Orientation = orientation
        flag.Charged = ModuleChargeState.Demo
        TaskWorkflow.execute(registerVanuModuleAssociatedWithSpawner(spawner, flag))
      case _ => ()
    }
  }

  private def registerVanuModuleAssociatedWithSpawner(spawner: VanuModuleNode, flag: VanuModule): TaskBundle = {
    // Register object create task and callback to create on clients
    TaskBundle(
      new StraightforwardTask() {
        private val func: (PlanetSideGUID, VanuModuleNode, VanuModule) => Unit = onSpawnBehaviors
        private val localFlag = flag
        private val localSocket = spawner

        override def description(): String = s"register a ${localSocket.ValidFlagType.value} module for socket"

        def action(): Future[Any] = {
          func(localSocket.GUID, localSocket, localFlag)
          Future(true)
        }
      },
      List(GUIDTask.registerObject(zone.GUID, flag))
    )
  }

  private def registerVanuModuleUponZoneEntry(flag: VanuModule): TaskBundle = {
    // Register object create task and callback to create on clients
    TaskBundle(
      new StraightforwardTask() {
        private val func: (Zone, VanuModule) => Unit = onEntryBehavior
        private val localFlag = flag
        private val localZone = zone

        override def description(): String = s"register vanu module flag ${localFlag.Definition.Name}"

        def action(): Future[Any] = {
          func(localZone, localFlag)
          Future(true)
        }
      },
      List(GUIDTask.registerObject(zone.GUID, flag))
    )
  }

  private def onSpawnBehaviors(
                                socketGuid: PlanetSideGUID,
                                socket: VanuModuleNode,
                                flag: VanuModule
                              ): Unit = {
    val zone = socket.Zone
    val zoneid = zone.id
    val owner = socket.Owner.asInstanceOf[Building]
    owner.Amenities = flag
    flag.Target = owner
    flag.Faction = socket.Faction
    socket.captureFlag = flag
    trackModule(flag)
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.LLUMaterialize, flag.Position, 15, volume = 0.8f)),
      MessageEnvelope(zoneid, GenericObjectAction(
        socketGuid,
        GenericObjectActionEnum.FlagSpawned.id
      )),
      MessageEnvelope(zoneid, SendResponse(OCM.apply(flag)))
    )
    owner.Actor ! BuildingActor.AmenityStateChange(socket)
    //todo broadcast chat message for module spawn?
    zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(socketGuid, flag.GUID, 0)))
  }

  private def onEntryBehavior(
                               thisZone: Zone,
                               flag: VanuModule
                             ): Unit = {
    val zoneid = thisZone.id
    val events = thisZone.LocalEvents
    val flagGuid = flag.GUID
    lazy val flagCreationPacketMsg = MessageEnvelope(zoneid, SendResponse(OCM.apply(flag)))
    flag.Zone = zone
    trackModule(flag)
    flag.Carrier
      .map { carrier =>
        val flagCategory = flag.ValidFlagType.category
        flag.Carrier = Some(carrier)
        carrier.Carrying = Some(flagCategory)
        flag.Position = carrier.Position + FlagHoverHeight
        events ! BundledEnvelope(
          flagCreationPacketMsg,
          MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(carrier.GUID, flagGuid, Player.SpecialCarrySlot))),
          MessageEnvelope(carrier.Name, LocalAction.PlayerCollectsFlag(flagCategory, flagGuid))
        )
        Some(carrier)
      }
      .getOrElse {
        events ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.LLUMaterialize, flag.Position, 15, volume = 0.8f)),
          flagCreationPacketMsg
        )
      }
  }

  private def tryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) if cradle.ValidFlagType == CarriableFlag.VanuModuleCradle =>
        RemoveModuleFromFactionOwnership(flag.Faction)
        tryDespawnModuleAssociatedWithCradle(cradle, flag)
      case Some(flag: VanuModule) =>
        tryDespawnModuleAssociatedWithCradle(cradle, flag)
      case _ =>
        ()
    }
  }

  private def tryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode, module: VanuModule): Unit = {
    val cradleOwner = cradle.Owner
    cradle.captureFlag = None
    cradleOwner.RemoveAmenity(module)
    cradleOwner.Actor ! BuildingActor.AmenityStateChange(cradle)
    handleFlagDespawnLocal(module)
  }

  private def handleFlagDespawnIfCarried(module: VanuModule): Unit = {
    module.Carrier match {
      case Some(carrier) =>
        module.Carrier = None
        carrier.Carrying = None
        module.Position = carrier.Position + FlagHoverHeight
        carrier.Zone.AvatarEvents ! MessageEnvelope(carrier.Name, LocalAction.PlayerDropsFlag(module.GUID))
        context.system.scheduler.scheduleOnce(1000 milliseconds, self, ModuleLost(module))
      case None =>
        handleFlagDespawnLocal(module)
    }
  }

  private def handleFlagDespawnLocal(flag: VanuModule): Unit = {
    val zone = flag.Zone
    if (flag.Destroyed) {
      untrackModule(flag)
      zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(OCM.apply(flag))) //explosion
      //todo cause explosion damage
      context.system.scheduler.scheduleOnce(500 milliseconds, self, ModuleLost(flag))
      flag.Destroyed = false //reset
    } else {
      zone.LocalEvents ! BundledEnvelope(
        MessageEnvelope(zone.id, LocalAction.TriggerSound(TriggeredSound.LLUDeconstruct, flag.Position, unk=20, volume=0.8000001f)),
        MessageEnvelope(zone.id, ObjectDelete(flag.GUID))
      )
      TaskWorkflow.execute(GUIDTask.unregisterObject(zone.GUID, flag))
    }
  }

  private def canHaveMoreRoamingModulesInZone: Boolean = {
    trackedModules.count(_.Target == Building.NoBuilding) < zone.Info.moduleRoamingLimit
  }

  private def trackModule(module: VanuModule): Boolean = {
    trackedModules.indexWhere(flag => module eq flag) match {
      case -1 =>
        trackedModules = trackedModules :+ module
        if (vanuModuleUpdateTimer.isCancelled) {
          //see DisplayVanuModuleIconsInZone for where the display timer is cancelled
          vanuModuleUpdateTimer = context.system.scheduler.scheduleAtFixedRate(
            initialDelay = 1000 milliseconds,
            interval = 1000 milliseconds,
            self,
            DisplayVanuModuleIconsInZone
          )
        }
      case _ => ()
    }
    true
  }

  private def newModuleInZone(module: VanuModule, host: Player): Unit = {
    if ((module.Charged == ModuleChargeState.Charged || (module.Owner.Zone eq zone)) && canHaveMoreRoamingModulesInZone) {
      TaskWorkflow.execute(registerVanuModuleUponZoneEntry(module))
    } else {
      module.Destroyed = true
      //module does not enter zone
      if (module.Carrier.contains(host)) {
        module.Carrier = None
        host.Carrying = None
        VanuModuleNode.ThatSpawnedThis(module).ClearModule()
      }
    }
  }

  private def untrackModule(module: VanuModule): Boolean = {
    trackedModules.indexWhere(flag => module eq flag) match {
      case -1 =>
        false
      case index =>
        //see DisplayVanuModuleIconsInZone for where the display timer is cancelled
        trackedModules = trackedModules.take(index) ++ trackedModules.drop(index + 1)
        true
    }
  }
}
