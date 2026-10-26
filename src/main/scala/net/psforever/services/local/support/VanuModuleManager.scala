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
import net.psforever.objects.serverobject.flag.module.{ModuleChargeState, VanuModule, VanuModuleNode, VanuModuleSpawnerControl}
import net.psforever.objects.serverobject.structures.Building
import net.psforever.objects.zones.Zone
import net.psforever.packet.game.packets.{GenericObjectActionEnum, ObjectAttachMessage, ObjectDetachMessage, TriggeredSound}
import net.psforever.services.ServiceManager.{Lookup, LookupResult}
import net.psforever.services.avatar.AvatarAction
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.{GenericObjectAction, SelfRespondingEvent, SendResponse}
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
  final val FlagHoverHeight: Vector3 = Vector3.z(value = 1.8f)

  trait Command

  final case class ModuleSpawnerReady(obj: VanuModuleNode) extends Command

  final case class ModuleSpawnerUnready(obj: VanuModuleNode) extends Command

  final case class TrackModuleInZone(module: VanuModule) extends Command

  final case class StopTrackingModule(module: VanuModule) extends Command

  final case class WhichModulesToDisplay(channel: String) extends Command

  final case class DropModule(flag: OwnedFlag) extends Command

  final case class PickupModuleFromCradle(spawner: VanuModuleNode, player: Player) extends Command

  final case class PickUpModule(flag: OwnedFlag, player: Player) extends Command

  final case class StartChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class StopChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeComplete(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeModule(module: VanuModule) extends Command

  final case class ModuleLost(module: VanuModule) extends Command

  final case class InstallModuleIntoCradle(cradle: VanuModuleNode, module: VanuModule, player: Player) extends Command

  final case class FreeModuleFromCradle(cradle: VanuModuleNode) extends Command

  private case object DisplayVanuModuleIconsInZone extends Command

  def CradleReadyToFunctionStatus(cradle: VanuModuleNode): Boolean = {
    cradle.Faction != PlanetSideEmpire.NEUTRAL &&
      cradle.Owner.asInstanceOf[Building].CaptureTerminal.forall(_.HackedBy.isEmpty)
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

    case StartChargingModule(module, _, player) =>
      //todo charge pose
      module.Charged = ModuleChargeState.Charging
      player.Actor ! Damageable.MakeInvulnerable

    case StopChargingModule(module, _, player) =>
      //todo undo charge pose
      module.Charged = ModuleChargeState.NonPowered
      player.Actor ! Damageable.MakeVulnerable

    case ChargeComplete(module, beam, player) =>
      chargeComplete(module, beam, player)

    case ChargeModule(flag)
      if flag.Charged == ModuleChargeState.Charging =>
      chargeModule(flag)

    case ModuleLost(flag) =>
      handleFlagDespawnLocal(flag)

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
      }
    spawner.captureFlag.collect { case o: VanuModule => trackModule(o) }
  }

  private def removeSpawner(spawner: VanuModuleNode): Unit = {
    val spawnerGuid = spawner.GUID
    cradles.indexWhere(_.GUID == spawnerGuid) match {
      case -1 => ()
      case index =>
        cradles = cradles.take(index) ++ cradles.drop(index + 1)
    }
    spawner.captureFlag.collect { case o: VanuModule => untrackModule(o) }
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
      .filterNot(_.Destroyed)
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
            if (results.isInstalled) {
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
    updateVanuModuleIcons()
    if (trackedModules.isEmpty) {
      vanuModuleUpdateTimer.cancel()
      vanuModuleUpdateTimer = Default.Cancellable
    }
  }

  private def pickupModuleFromCradle(spawner: VanuModuleNode, player: Player): Unit = {
    spawner.captureFlag match {
      case Some(flag: VanuModule) =>
        val zoneid = flag.Zone.id
        val flagGuid = flag.GUID
        val playerPosition = player.Position
        player.Carrying = flag.ValidFlagType.category
        flag.Carrier = Some(player)
        flag.Target = Building.NoBuilding
        flag.Charged = false
        flag.Position = Vector3.Zero //spawn far out of sight, at first
        val flagOcmPacket = OCM.apply(flag)
        flag.Position = playerPosition
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, SendResponse(
            ObjectDetachMessage(spawner.GUID, flagGuid, spawner.Position, 0, 0, 0), /* spawner emits typed module */
            flagOcmPacket, /* overwrite with nonpowered module */
            ObjectAttachMessage(player.GUID, flagGuid, Player.SpecialCarrySlot) /* nonpowered module held by player */
          )),
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, playerPosition, 15, volume = 0.8f))
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
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(player.GUID, flag.GUID, flagPosition, 0, 0, flagOrientation.z))),
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, playerPosition, 15, volume = 0.8f))
        )
      case _ => ()
    }
  }

  private def pickUpModule(flag: OwnedFlag, player: Player): Unit = {
    flag.Carrier match {
      case None if !flag.Destroyed =>
        val zoneid = flag.Zone.id
        flag.Carrier = Some(player)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(player.GUID, flag.GUID, Player.SpecialCarrySlot))),
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, player.Position, 15, volume = 0.8f))
        )
      case _ => ()
    }
  }

  private def chargeComplete(module: VanuModule, @unused beam: VanuModuleBeam, player: Player): Unit = {
    val zoneid = zone.id
    module.Charged = true
    VanuModuleNode.ThatSpawnedThis(module).ClearModule()
    val playerPosition = player.Position
    module.Position = Vector3.Zero //spawn far out of sight
    val flagOcmPacket = OCM.apply(module)
    module.Position = playerPosition
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, SendResponse(
        flagOcmPacket, /* overwrite with charged module */
        ObjectAttachMessage(player.GUID, module.GUID, Player.SpecialCarrySlot) /* charged module held by player */
      )),
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, playerPosition, 15, volume = 0.8f))
    )
    //todo undo charge pose
    player.Actor ! Damageable.MakeVulnerable
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
          MessageEnvelope(zoneid, SendResponse(
            flagOcmPacket, /* overwrite with charged module */
            ObjectAttachMessage(player.GUID, flag.GUID, Player.SpecialCarrySlot) /* charged module held by player */
          )),
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, playerPosition, 15, volume = 0.8f))
        )
        //todo undo charge pose
        player.Actor ! Damageable.MakeVulnerable
      case None =>
        //we would not normally charge flags that are not being carried, but ...
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, SendResponse(OCM.apply(flag))), /* overwrite with charged module */
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, flag.Position, 15, volume = 0.8f))
        )
    }
  }

  private def installModuleIntoCradle(cradle: VanuModuleNode, flag: VanuModule, player: Player): Unit = {
    cradle.captureFlag.getOrElse {
      val zone = cradle.Zone
      val owner = cradle.Owner.asInstanceOf[Building]
      val flagPosition = cradle.Position + FlagHoverHeight
      val flagOrientation = cradle.Orientation
      flag.Position = flagPosition
      flag.Orientation = flagOrientation
      player.Carrying = None
      flag.Target = owner
      flag.Carrier = None
      cradle.captureFlag = flag
      cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
      zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(cradle.GUID, flag.GUID, 0)))
    }
  }

  private def freeModuleFromCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag) =>
        val zone = cradle.Zone
        val flagPosition = cradle.Position + FlagHoverHeight
        val flagOrientation = cradle.Orientation
        cradle.captureFlag = None
        flag.Target = Building.NoBuilding
        flag.Position = flagPosition
        flag.Orientation = flagOrientation
        zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectDetachMessage(cradle.GUID, flag.GUID, flagPosition, 0, 0, flagOrientation.z)))
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

  private def registerVanuModuleAssociatedWithSpawner(spawner: VanuModuleNode, flag: VanuModule) = {
    // Register object create task and callback to create on clients
    TaskBundle(
      new StraightforwardTask() {
        private val func: (PlanetSideGUID, VanuModuleNode, VanuModule) => Unit = onSpawnBehaviors
        private val localFlag = flag
        private val localSocket = spawner

        override def description(): String = s"register a ${localSocket.ValidFlagType.value} for socket"

        def action(): Future[Any] = {
          func(localSocket.GUID, localSocket, localFlag)
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
    val owner = socket.Owner.asInstanceOf[Building]
    flag.Target = owner
    owner.Amenities = flag
    socket.captureFlag = flag
    trackModule(flag)
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zone.id, GenericObjectAction(
        socketGuid,
        GenericObjectActionEnum.FlagSpawned.id
      )),
      MessageEnvelope(zone.id, LocalAction.LluSpawned(flag))
    )
    owner.Actor ! BuildingActor.AmenityStateChange(socket)
    //todo broadcast chat message for module spawn?
    zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(socketGuid, flag.GUID, 0)))
  }

  private def tryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) if flag.Charged != ModuleChargeState.Charged =>
        flag.Carrier match {
          case None =>
            handleFlagDespawnLocal(cradle, flag)
          case Some(carrier) =>
            carrier.Zone.AvatarEvents ! MessageEnvelope(carrier.Name, AvatarAction.DropSpecialItem())
            context.system.scheduler.scheduleOnce(1000 milliseconds, self, VanuModuleSpawnerControl.ClearModule)
        }
      case _ =>
        cradle.captureFlag = None
    }
  }

  private def handleFlagDespawnLocal(cradle: VanuModuleNode, flag: VanuModule): Unit = {
    val flagOwner = cradle.Owner.asInstanceOf[Building]
    cradle.captureFlag = None
    flagOwner.Actor ! BuildingActor.AmenityStateChange(cradle)
    flagOwner.RemoveAmenity(flag)
    handleFlagDespawnLocal(flag)
  }

  private def handleFlagDespawnLocal(flag: VanuModule): Unit = {
    val zone = flag.Zone
    flag.Destroyed = true
    untrackModule(flag)
    zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.LluDespawned(flag.GUID, flag.Position))
    TaskWorkflow.execute(GUIDTask.unregisterObject(zone.GUID, flag))
  }

  private def trackModule(module: VanuModule): Unit = {
    trackedModules.indexWhere(flag => module == flag || flag.GUID == module.GUID) match {
      case -1 =>
        trackedModules = trackedModules :+ module
        if (vanuModuleUpdateTimer.isCancelled) {
          vanuModuleUpdateTimer = context.system.scheduler.scheduleAtFixedRate(
            initialDelay = 1000 milliseconds,
            interval = 1000 milliseconds,
            self,
            DisplayVanuModuleIconsInZone
          )
          //see DisplayVanuModuleIconsInZone for where the display timer is cancelled
        }
      case _ => ()
    }
  }

  private def untrackModule(module: VanuModule): Unit = {
    trackedModules.indexWhere(flag => module eq flag) match {
      case -1 => ()
      case index =>
        trackedModules = trackedModules.take(index) ++ trackedModules.drop(index + 1)
        //see DisplayVanuModuleIconsInZone for where the display timer is cancelled
    }
  }

  private def updateVanuModuleIcons(): Unit = {
    val outputList = trackedModules
      .filterNot(_.Destroyed)
      .map { module => (module.DisplayFlagType, module.Faction, 0L, module.Position.xy) }
    galaxyService ! MessageEnvelope("", VanuModuleUpdate(zone.Number, outputList))
  }
}
