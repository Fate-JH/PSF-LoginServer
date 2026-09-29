// Copyright (c) 2026 PSForever
package net.psforever.services.local.support

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorContext, ActorRef, Props}
import net.psforever.objects.serverobject.flag.module.{VanuModuleNode, VanuModuleSpawnerControl}
import net.psforever.objects.zones.Zone
import net.psforever.services.{ServiceManager, VanuModuleRotationService}
import net.psforever.services.base.{EventServiceSupport, GenericSupportEnvelopeOnly}

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
      cradles
        .find(_.GUID == spawner.GUID)
        .orElse {
          cradles = cradles :+ spawner
          None
        }

    case ModuleSpawnerUnready(spawner) =>
      cradles.indexWhere(_.GUID == spawner.GUID) match {
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

    case _ => ()
  }
}
