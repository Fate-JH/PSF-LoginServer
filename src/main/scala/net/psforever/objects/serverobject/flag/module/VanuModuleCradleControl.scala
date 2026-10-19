// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.ActorRef
import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.Player
import net.psforever.objects.serverobject.structures.{Building, PoweredAmenityControl}
import net.psforever.packet.game.packets.GenericObjectActionEnum
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.GenericObjectAction
import net.psforever.services.local.LocalAction
import net.psforever.services.local.support.{VanuModuleEnvelope, VanuModuleManager}

object VanuModuleCradleControl {
  final case class InstallModule(flag: VanuModule, player: Player)

  case object FreeModule
}

class VanuModuleCradleControl(obj: VanuModuleNode)
  extends PoweredAmenityControl {
  override def poweredStateLogic: Receive = {
    case VanuModuleCradleControl.InstallModule(flag, player) =>
      obj.Zone.LocalEvents ! VanuModuleEnvelope(VanuModuleManager.InstallFlagIntoCradle(flag, obj, player))

    case VanuModuleCradleControl.FreeModule =>
      TryFreeModuleFromCradle(sender())

    case _ => ()
  }

  override def unpoweredStateLogic: Receive = {
    case _ => ()
  }

  override def powerTurnOnCallback(): Unit = { /* nothing */ }

  override def powerTurnOffCallback(): Unit = {
    TryFreeModuleFromCradle()
  }

  private def TryFreeModuleFromCradle(): Boolean = {
    obj.captureFlag match {
      case Some(flag) =>
        obj.captureFlag = None
        flag.Target = Building.NoBuilding
        val zone = obj.Zone
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zone.id, GenericObjectAction(
            obj.GUID,
            GenericObjectActionEnum.FlagSpawned.id
          )),
          MessageEnvelope(zone.id, LocalAction.LluSpawned(flag))
        )
        obj.Owner.Actor ! BuildingActor.AmenityStateChange(obj)
        true
      case _ =>
        false
    }
  }

  private def TryFreeModuleFromCradle(replyTo: ActorRef): Unit = {
    if (TryFreeModuleFromCradle()) {
      replyTo ! BuildingActor.AmenityStateChange(obj)
    }
  }
}
