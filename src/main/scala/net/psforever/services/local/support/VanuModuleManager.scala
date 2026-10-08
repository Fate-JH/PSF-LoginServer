// Copyright (c) 2026 PSForever
package net.psforever.services.local.support

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorContext, ActorRef, Props}
import net.psforever.objects.Player
import net.psforever.objects.definition.converter.OCM
import net.psforever.objects.serverobject.flag.base.OwnedFlag
import net.psforever.objects.serverobject.flag.module.{ChargedState, VanuModule, VanuModuleNode, VanuModuleSpawnerControl}
import net.psforever.objects.zones.Zone
import net.psforever.packet.game.packets.{ObjectAttachMessage, ObjectDetachMessage, TriggeredSound}
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.SendResponse
import net.psforever.services.{ServiceManager, VanuModuleRotationService}
import net.psforever.services.base.{EventServiceSupport, GenericSupportEnvelopeOnly}
import net.psforever.services.local.LocalAction
import net.psforever.types.Vector3

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

object VanuModuleManager {
  trait Command

  final case class ModuleSpawnerReady(obj: VanuModuleNode) extends Command

  final case class ModuleSpawnerUnready(obj: VanuModuleNode) extends Command

  final case class DropFlag(flag: OwnedFlag) extends Command

  final case class PickupFlagFromCradle(flag: OwnedFlag, spawner: VanuModuleNode, player: Player) extends Command

  final case class PickupFlag(flag: OwnedFlag, player: Player) extends Command

  final case class ChargeFlag(flag: VanuModule) extends Command
}

class VanuModuleManager(zone: Zone) extends Actor {
  import VanuModuleManager._

  private var cradles: Seq[VanuModuleNode] = List()

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

    case ModuleSpawnerUnready(spawner) =>
      val spawnerGuid = spawner.GUID
      cradles.indexWhere(_.GUID == spawnerGuid) match {
        case -1 => ()
        case index =>
          cradles = cradles.take(index) ++ cradles.drop(index + 1)
      }
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
        .foreach(_.Actor ! VanuModuleSpawnerControl.SpawnModule)
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
        .foreach(_.Actor ! VanuModuleSpawnerControl.ClearModule)
      cradles
        .filter(o => modulesToActivate.contains(o.ValidFlagType) && o.captureFlag.isEmpty)
        .foreach(_.Actor ! VanuModuleSpawnerControl.SpawnModule)

    case PickupFlagFromCradle(flag, spawner, player) =>
      val zoneid = flag.Zone.id
      val flagGuid = flag.GUID
      val playerPosition = player.Position
      flag.Carrier = Some(player)
      flag.asInstanceOf[VanuModule].Charged = false
      flag.Position = Vector3.Zero //spawn far out of sight
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
            //todo? MessageEnvelope(zoneid, LocalAction.LluSpawned(flag)),
            MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, playerPosition, 15, volume = 0.8f))
          )
        case _ => ()
      }

    case PickupFlag(flag, player) =>
      flag.Carrier match {
        case None =>
          val zoneid = flag.Zone.id
          flag.Carrier = Some(player)
          zone.LocalEvents ! BundledEnvelope(
            MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(player.GUID, flag.GUID, Player.SpecialCarrySlot))),
            MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, player.Position, 15, volume = 0.8f))
          )
        case _ => ()
      }

    case ChargeFlag(flag)
      if flag.Charged == ChargedState.NonPowered =>
      val zoneid = zone.id
      flag.Charged = true
      flag.Owner
        .Amenities
        .collectFirst { case o: VanuModuleNode if o.captureFlag.contains(flag) => o }
        .map(_.captureFlag = None)
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
        case None =>
          //we would not normally charge flags that are not being carried, but ...
          zone.LocalEvents ! BundledEnvelope(
            MessageEnvelope(zoneid, SendResponse(OCM.apply(flag))), /* overwrite with charged module */
            MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, flag.Position, 15, volume = 0.8f))
          )
      }

    case _ => ()
  }
}
