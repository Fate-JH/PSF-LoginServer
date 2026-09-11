// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.{Actor, Cancellable}
import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.{Default, GlobalDefinitions}
import net.psforever.objects.guid.{GUIDTask, StraightforwardTask, TaskBundle, TaskWorkflow}
import net.psforever.objects.serverobject.flag.base.OwnedFlag
import net.psforever.objects.serverobject.structures.Building
import net.psforever.objects.serverobject.terminals.capture.{CaptureTerminal, CaptureTerminalAwareBehavior}
import net.psforever.packet.game.packets.{GenericObjectActionEnum, ObjectAttachMessage}
import net.psforever.services.avatar.AvatarAction
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.{GenericObjectAction, SendResponse}
import net.psforever.services.local.LocalAction
import net.psforever.types.{PlanetSideEmpire, PlanetSideGUID, Vector3}

import scala.annotation.unused
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class VanuModuleSpawnerControl(obj: VanuModuleNode)
  extends Actor
    with CaptureTerminalAwareBehavior {
  //private implicit val timeout: Timeout = new Timeout(5000 milliseconds)

  private var timeUntilSpawn: Long = 2160000 //ms (6 hours)
  private var spawnTimer: Cancellable = Default.Cancellable

  def CaptureTerminalAwareObject: VanuModuleNode = obj

  val receive: Receive = captureTerminalAwareBehaviour
      .orElse {
        case VanuModuleSpawnerControl.SetSpawnTimer(time) =>
          spawnTimer.cancel()
          timeUntilSpawn = time
          TryStartSpawnTimer()

        case VanuModuleSpawnerControl.ClearModule =>
          TryDespawnVanuModule()

        case VanuModuleSpawnerControl.SpawnModule =>
          TrySpawnModule()

        case _ => ()
      }

  override protected def captureTerminalIsHacked(@unused terminal: CaptureTerminal): Unit = {
    if (terminal.HackedBy.isEmpty) {
      //our faction identity was manually set, this is not an actual facility hack
    }
  }

  override protected def captureTerminalIsResecured(@unused terminal: CaptureTerminal): Unit = {
    val zone = obj.Zone
    obj.captureFlag match {
      case Some(flag: VanuModule)
        if flag.Faction != terminal.Faction && flag.Zone == zone =>
        //module is out in the cavern, hack was completed
        flag.Destroyed = true
        flag.Carrier match {
          case Some(carrier) =>
            flag.Position = carrier.Position + Vector3.z(value = 0.5f)
            //todo server-side explosion, or damage just to carrier?
          case None => ()
            zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.LluSpawned(flag)) //explosion animation
        }
        HandleFlagDespawn(flag)
      case None
        if terminal.Faction != PlanetSideEmpire.NEUTRAL =>
        //spawn module attempt
        TrySpawnModule()
      case None | Some(_) => () //either module is in another zone, or the defenders resecured a hacked facility
    }
  }

  private def TryStartSpawnTimer(): Unit = {
    obj.captureFlag match {
      case None if spawnTimer.isCancelled =>
        spawnTimer = context.system.scheduler.scheduleOnce(timeUntilSpawn milliseconds, self, VanuModuleSpawnerControl.SpawnModule)
      case _ => ()
    }
  }

  private def TrySpawnModule(): Unit = {
    obj.captureFlag match {
      case None =>
        RegisterAndSpawnVanuModule(obj, obj.Position, obj.Orientation)
      case _ => ()
    }
  }

  private def TryDespawnVanuModule(): Boolean = {
    obj.captureFlag match {
      case Some(flag: VanuModule) if !flag.Charged =>
        HandleFlagDespawn(flag)
        true
      case _ =>
        false
    }
  }

  private def RegisterAndSpawnVanuModule(
                                          spawner: VanuModuleNode,
                                          position: Vector3,
                                          orientation: Vector3
                                        ): Unit = {
    // Construct new flag
    val zone = spawner.Zone
    val flag = new VanuModule(GlobalDefinitions.vanu_module, spawner.ValidFlagType)
    flag.Zone = spawner.Zone
    flag.Position = position + Vector3.z(value = 1f)
    flag.Orientation = orientation
    flag.Owner = spawner.Owner
    flag.Faction = spawner.Faction
    // Register object create task and callback to create on clients
    TaskWorkflow.execute(
      TaskBundle(
        new StraightforwardTask() {
          private val func: () => Unit = OnSpawnBehaviors(spawner.GUID, spawner, flag)
          private val localSocket = spawner

          override def description(): String = s"register a ${localSocket.ValidFlagType.value} for socket"

          def action(): Future[Any] = {
            func()
            Future(true)
          }
        },
        List(GUIDTask.registerObject(zone.GUID, flag))
      )
    )
  }

  private def OnSpawnBehaviors(
                                captureTerminalGuid: PlanetSideGUID,
                                socket: VanuModuleNode,
                                flag: OwnedFlag
                              )(): Unit = {
    val zone = socket.Zone
    val owner = socket.Owner.asInstanceOf[Building]
    // Add the flag as an amenity
    owner.Amenities = flag
    socket.captureFlag = flag
    // Track new flag
    //todo TrackFlag(flag)

    // 1.Override CC message when looked at, 2.Announce flag spawn
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zone.id, GenericObjectAction(
        captureTerminalGuid,
        GenericObjectActionEnum.FlagSpawned.id
      )),
      MessageEnvelope(zone.id, LocalAction.LluSpawned(flag))
    )
    owner.Actor ! BuildingActor.AmenityStateChange(obj)
    // Broadcast chat message for LLU spawn
    //todo ???
    zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(socket.GUID, flag.GUID, 0))) //todo actual attach index
  }

  private def HandleFlagDespawn(flag: OwnedFlag): Unit = {
    // Remove the flag as an amenity
    flag.Target match {
      case Building.NoBuilding if flag.Carrier.isEmpty =>
        HandleFlagDespawnLocal(flag)
      case Building.NoBuilding => //being carried; must sort that out first
        val carrier = flag.Carrier.get
        carrier.Zone.AvatarEvents ! MessageEnvelope(carrier.Name, AvatarAction.DropSpecialItem())
        context.system.scheduler.scheduleOnce(1000 milliseconds, self, VanuModuleSpawnerControl.ClearModule)
      case someBuilding =>
        someBuilding
          .Amenities
          .collectFirst { case cradle: VanuModuleNode if cradle.captureFlag.contains(flag) => cradle }
          .foreach { cradle =>
            cradle.Actor ! VanuModuleCradleControl.FreeModule
            context.system.scheduler.scheduleOnce(1000 milliseconds, self, VanuModuleSpawnerControl.ClearModule)
          }
    }
  }

  private def HandleFlagDespawnLocal(flag: OwnedFlag): Unit = {
    val zone = flag.Zone
    val flagOwner = flag.Owner.asInstanceOf[Building]
    val socket = flagOwner
      .Amenities
      .collectFirst { case cradle: VanuModuleNode if cradle.captureFlag.contains(flag) => cradle }
    socket.map(_.captureFlag = None)
    flagOwner.RemoveAmenity(flag)
    flagOwner.Actor ! BuildingActor.AmenityStateChange(obj)
    //UntrackFlag(flag)
    // Unregister LLU from clients,
    zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.LluDespawned(flag.GUID, flag.Position))
    // Then unregister it from the GUID pool
    TaskWorkflow.execute(GUIDTask.unregisterObject(zone.GUID, flag))
  }
}

object VanuModuleSpawnerControl {
  final case class SetSpawnTimer(time: Long)

  case object SpawnModule

  case object ClearModule
}
