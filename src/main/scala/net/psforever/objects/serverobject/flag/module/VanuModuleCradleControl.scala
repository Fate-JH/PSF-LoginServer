// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import akka.actor.ActorRef
import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.serverobject.flag.base.OwnedFlag
import net.psforever.objects.serverobject.structures.{Building, PoweredAmenityControl}
import net.psforever.packet.game.GenericObjectActionEnum
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.GenericObjectAction
import net.psforever.services.local.LocalAction

class VanuModuleCradleControl(obj: VanuModuleNode)
  extends PoweredAmenityControl {
  override def poweredStateLogic: Receive = {
    case VanuModuleCradleControl.InstallModule(flag) if flag.Charged =>
      TryInstallModule(flag, sender())

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

  private def TryInstallModule(flag: OwnedFlag, replyTo: ActorRef): Unit = {
    obj.captureFlag.getOrElse {
      obj.captureFlag = flag
      flag.Target = obj.Owner.asInstanceOf[Building]
      flag.Carrier = None
      val msg = BuildingActor.AmenityStateChange(obj)
      obj.Owner.Actor ! msg
      replyTo ! msg
      flag
    }
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

object VanuModuleCradleControl {
  final case class InstallModule(flag: VanuModule)

  case object FreeModule
}
