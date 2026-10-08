// Copyright (c) 2026 PSForever
package net.psforever.services

import akka.actor.{ActorRef => ClassicActorRef}
import akka.actor.typed.{Behavior, PostStop, SupervisorStrategy}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import net.psforever.objects.serverobject.flag.base.CarriableFlag

import scala.concurrent.duration._

object VanuModuleRotationService {
  val VanuModuleRotationServiceKey: ServiceKey[Command] =
    ServiceKey[VanuModuleRotationService.Command](id = "vanuModuleRotationService")

  def apply(): Behavior[Command] =
    Behaviors
      .supervise[Command] {
        Behaviors.withTimers { timer =>
          Behaviors.setup { context =>
            context.system.receptionist ! Receptionist.Register(VanuModuleRotationServiceKey, context.self)
            new VanuModuleRotationService(context, timer).start()
        }
      }
    }.onFailure[Exception](SupervisorStrategy.restart)

  sealed trait Command

  final case object RotateAvailability extends Command

  final case class RotationHasOccurred(previous: Seq[CarriableFlag], current: Seq[CarriableFlag])

  final case class WhatCurrentModulesAreAvailable(sendTo: ClassicActorRef) extends Command

  final case class CurrentModulesAvailable(modules: Seq[CarriableFlag])

  final case class KeepUpdated(endpoint: ClassicActorRef) extends Command
}

class VanuModuleRotationService(
                             context: ActorContext[VanuModuleRotationService.Command],
                             timer: TimerScheduler[VanuModuleRotationService.Command]
                           ) {

  import VanuModuleRotationService._

  private val defaultModuleRotationOrder: Seq[CarriableFlag] = Seq(
    CarriableFlag.VanuModuleWeapon,
    CarriableFlag.VanuModuleVehicle,
    CarriableFlag.VanuModuleFortifier,
    CarriableFlag.VanuModuleBind,
    CarriableFlag.VanuModuleDefender,
    CarriableFlag.VanuModuleEnergy
  )
  private var currentIndex: Int = 0
  private val rotationSample: Int = 1
  private var currentlyAvailableModules: Seq[CarriableFlag] = defaultModuleRotationOrder.slice(currentIndex, currentIndex + rotationSample)
  private var managerEndpoints: Seq[ClassicActorRef] = List()

  def start(): Behavior[VanuModuleRotationService.Command] = {
    timer.startTimerWithFixedDelay(RotateAvailability, RotateAvailability, 300.seconds)

    Behaviors
      .receiveMessage[VanuModuleRotationService.Command] {
        case KeepUpdated(endpoint) =>
          managerEndpoints = managerEndpoints :+ endpoint
          endpoint ! CurrentModulesAvailable(currentlyAvailableModules)
          Behaviors.same

        case WhatCurrentModulesAreAvailable(sendTo) =>
          sendTo ! CurrentModulesAvailable(currentlyAvailableModules)
          Behaviors.same

        case RotateAvailability =>
          val previousModules: Seq[CarriableFlag] = currentlyAvailableModules
          val size: Int = defaultModuleRotationOrder.size
          currentIndex = (currentIndex + rotationSample) % size
          val nextIndex: Int = currentIndex + rotationSample
          currentlyAvailableModules = if (nextIndex >= size) {
            defaultModuleRotationOrder.slice(currentIndex, size + 1) ++ defaultModuleRotationOrder.slice(0, nextIndex - size)
          } else {
            defaultModuleRotationOrder.slice(currentIndex, nextIndex)
          }
          managerEndpoints.foreach(_ ! RotationHasOccurred(previousModules, currentlyAvailableModules))
          Behaviors.same
      }
      .receiveSignal {
        case (_, PostStop) =>
          timer.cancelAll()
          Behaviors.same
      }
  }
}
