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
import net.psforever.objects.serverobject.flag.module.{ChargedState, VanuModule, VanuModuleNode, VanuModuleSpawnerControl}
import net.psforever.objects.serverobject.structures.Building
import net.psforever.objects.zones.Zone
import net.psforever.packet.game.packets.{GenericObjectActionEnum, ObjectAttachMessage, ObjectDetachMessage, TriggeredSound}
import net.psforever.services.avatar.AvatarAction
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.{GenericObjectAction, SelfRespondingEvent, SendResponse}
import net.psforever.services.{ServiceManager, VanuModuleRotationService}
import net.psforever.services.base.{EventServiceSupport, GenericSupportEnvelopeOnly}
import net.psforever.services.local.LocalAction
import net.psforever.types.{PlanetSideEmpire, PlanetSideGUID, Vector3}

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

final case class VanuModuleUpdate(modules: Seq[(CarriableFlag, PlanetSideEmpire.Value, Long, Vector3)]) extends SelfRespondingEvent

object VanuModuleManager {
  trait Command

  final case class ModuleSpawnerReady(obj: VanuModuleNode) extends Command

  final case class ModuleSpawnerUnready(obj: VanuModuleNode) extends Command

  final case class TrackModuleInZone(module: VanuModule) extends Command

  final case class StopTrackingModule(module: VanuModule) extends Command

  final case class WhichModulesToDisplay(channel: String) extends Command

  final case class SpawnFlag(flag: VanuModule) extends Command

  final case class DespawnFlag(flag: VanuModule) extends Command

  final case class DropFlag(flag: OwnedFlag) extends Command

  final case class PickupFlagFromCradle(flag: VanuModule, spawner: VanuModuleNode, player: Player) extends Command

  final case class PickupFlag(flag: OwnedFlag, player: Player) extends Command

  final case class StartChargingFlag(flag: OwnedFlag, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeFlag(flag: VanuModule) extends Command

  final case class FlagLost(flag: VanuModule) extends Command

  final case class InstallFlagIntoCradle(flag: VanuModule, cradle: VanuModuleNode, player: Player) extends Command

  private case object DisplayVanuModuleIconsInZone extends Command
}

class VanuModuleManager(zone: Zone) extends Actor {
  import VanuModuleManager._

  private var cradles: Seq[VanuModuleNode] = Seq()
  private var trackedModules: Seq[VanuModule] = Seq()

  private var vanuModuleUpdateTimer: Cancellable = Default.Cancellable

  override def preStart(): Unit = {
    super.preStart()
    ServiceManager.receptionist ! Receptionist.Find(
      VanuModuleRotationService.VanuModuleRotationServiceKey,
      context.self.toTyped
    )
  }

  val AddRemoveCradles: Receive = {
    case ModuleSpawnerReady(spawner) =>
      val spawnerGuid = spawner.GUID
      cradles
        .find(_.GUID == spawnerGuid)
        .getOrElse {
          cradles = cradles :+ spawner
        }
      spawner.captureFlag.collect { case o: VanuModule => TrackModule(o) }

    case ModuleSpawnerUnready(spawner) =>
      val spawnerGuid = spawner.GUID
      cradles.indexWhere(_.GUID == spawnerGuid) match {
        case -1 => ()
        case index =>
          cradles = cradles.take(index) ++ cradles.drop(index + 1)
      }
      spawner.captureFlag.collect { case o: VanuModule => UntrackModule(o) }
  }

  def receive: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.VanuModuleRotationServiceKey.Listing(listings) =>
      val manager = listings.head
      manager ! VanuModuleRotationService.KeepUpdated(self)
      context.become(setInitialSpawners)

    case _ => ()
  }

  private val setInitialSpawners: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.CurrentModulesAvailable(currentlyAvailableModules) =>
      cradles
        .filter(o => currentlyAvailableModules.contains(o.ValidFlagType) && o.captureFlag.isEmpty)
        .foreach(TrySpawnModuleInCradle)
      context.become(standardRotation)

    case _ => ()
  }

  private val standardRotation: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.RotationHasOccurred(previousModules, currentlyAvailableModules) =>
      val previousModuleSet = previousModules.toSet
      val modulesToActivate = currentlyAvailableModules.toSet.diff(previousModuleSet)
      val modulesToClear = previousModuleSet.diff(modulesToActivate)
      cradles
        .filter(o => modulesToClear.contains(o.ValidFlagType) && o.captureFlag.nonEmpty)
        .foreach(TryDespawnModuleAssociatedWithCradle)
      cradles
        .filter(o => modulesToActivate.contains(o.ValidFlagType) && o.captureFlag.isEmpty)
        .foreach(TrySpawnModuleInCradle)

    case TrackModuleInZone(module) =>
      TrackModule(module)

    case StopTrackingModule(module) =>
      UntrackModule(module)

    case WhichModulesToDisplay(channel) =>
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

    case DisplayVanuModuleIconsInZone =>
      UpdateVanuModuleIcons()
      if (trackedModules.isEmpty) {
        vanuModuleUpdateTimer.cancel()
        vanuModuleUpdateTimer = Default.Cancellable
      }

    case SpawnFlag(flag: VanuModule) =>
      TrackModule(flag)

    case DespawnFlag(flag: VanuModule) =>
      UntrackModule(flag)

    case PickupFlagFromCradle(flag, spawner, player) =>
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

    case DropFlag(flag) =>
      flag.Carrier match {
        case Some(player: Player) =>
          val zoneid = zone.id
          val playerPosition = player.Position
          val flagPosition = playerPosition + Vector3.z(value = 0.9f)
          flag.Position = flagPosition
          flag.Carrier = None
          zone.LocalEvents ! BundledEnvelope(
            MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(player.GUID, flag.GUID, flagPosition, 0, 0, 0))),
            MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, playerPosition, 15, volume = 0.8f))
          )
        case _ => ()
      }

    case PickupFlag(flag, player) =>
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

    case StartChargingFlag(_, _, player) =>
      //todo charge pose
      player.Actor ! Damageable.MakeInvulnerable

    case ChargeFlag(flag)
      if flag.Charged == ChargedState.NonPowered =>
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

    case FlagLost(flag) =>
      HandleFlagDespawnLocal(flag)

    case InstallFlagIntoCradle(flag, cradle, player) =>
      cradle.captureFlag.getOrElse {
        val zone = cradle.Zone
        val owner = cradle.Owner.asInstanceOf[Building]
        player.Carrying = None
        flag.Target = owner
        flag.Carrier = None
        cradle.captureFlag = flag
        cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
        zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(cradle.GUID, flag.GUID, 0)))
      }

    case _ => ()
  }

  private def TrySpawnModuleInCradle(spawner: VanuModuleNode): Unit = {
    spawner.captureFlag match {
      case None =>
        // Construct new flag
        val zone = spawner.Zone
        val flag = new VanuModule(GlobalDefinitions.vanu_module, spawner.ValidFlagType)
        val position = spawner.Position
        val orientation = spawner.Orientation
        flag.Zone = zone
        flag.Position = position + Vector3.z(value = 1f)
        flag.Orientation = orientation
        flag.Charged = ChargedState.Demo
        // Register object create task and callback to create on clients
        TaskWorkflow.execute(
          TaskBundle(
            new StraightforwardTask() {
              private val func: (PlanetSideGUID, VanuModuleNode, VanuModule) => Unit = OnSpawnBehaviors
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
        )
      case _ => ()
    }
  }

  private def OnSpawnBehaviors(
                                socketGuid: PlanetSideGUID,
                                socket: VanuModuleNode,
                                flag: VanuModule
                              ): Unit = {
    val zone = socket.Zone
    val owner = socket.Owner.asInstanceOf[Building]
    flag.Target = owner
    owner.Amenities = flag
    socket.captureFlag = flag
    TrackModule(flag)
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

  private def TryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) if flag.Charged != ChargedState.Charged =>
        flag.Carrier match {
          case None =>
            HandleFlagDespawnLocal(cradle, flag)
          case Some(carrier) =>
            carrier.Zone.AvatarEvents ! MessageEnvelope(carrier.Name, AvatarAction.DropSpecialItem())
            context.system.scheduler.scheduleOnce(1000 milliseconds, self, VanuModuleSpawnerControl.ClearModule)
        }
      case _ =>
        cradle.captureFlag = None
    }
  }

  private def HandleFlagDespawnLocal(cradle: VanuModuleNode, flag: VanuModule): Unit = {
    val flagOwner = cradle.Owner.asInstanceOf[Building]
    cradle.captureFlag = None
    flagOwner.Actor ! BuildingActor.AmenityStateChange(cradle)
    flagOwner.RemoveAmenity(flag)
    HandleFlagDespawnLocal(flag)
  }

  private def HandleFlagDespawnLocal(flag: VanuModule): Unit = {
    val zone = flag.Zone
    flag.Destroyed = true
    UntrackModule(flag)
    zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.LluDespawned(flag.GUID, flag.Position))
    TaskWorkflow.execute(GUIDTask.unregisterObject(zone.GUID, flag))
  }

  private def TrackModule(module: VanuModule): Unit = {
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
        }
      case _ => ()
    }
  }

  private def UntrackModule(module: VanuModule): Unit = {
    trackedModules.indexWhere(flag => module == flag || flag.GUID == module.GUID) match {
      case -1 => ()
      case index =>
        trackedModules = trackedModules.take(index) ++ trackedModules.drop(index + 1)
        //see DisplayVanuModuleIconsInZone for where the display timer can be cancelled
    }
  }

  private def UpdateVanuModuleIcons(): Unit = {
    val outputList = trackedModules
      .filterNot(_.Destroyed)
      .map { module =>
        (
          module.DisplayFlagType,
          module.Carrier.map(_.Faction).getOrElse(PlanetSideEmpire.NEUTRAL),
          0L,
          module.Position.xy
        )
      }
    zone.LocalEvents ! MessageEnvelope(zone.id, VanuModuleUpdate(outputList))
  }
}
