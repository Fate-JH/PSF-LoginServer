package net.psforever.actors.zone

import akka.actor.Cancellable
import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.{TypedActorContextOps, TypedActorRefOps}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext}
import net.psforever.actors.session.SessionActor
import net.psforever.objects.{Default, LocalLockerItem, LocalProjectile, PlanetSideGameObject}
import net.psforever.objects.guid.NumberPoolHub
import net.psforever.objects.zones.Zone
import org.log4s.Logger

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object GuidBeanCounter {
  def apply(zone: Zone, numberSource: NumberPoolHub): Behavior[Command] =
    Behaviors.supervise[Command] {
      Behaviors.setup(context => new GuidBeanCounter(context, zone, numberSource))
    }
      .onFailure[Exception](SupervisorStrategy.restart)

  trait Command

  final case class Start() extends Command

  final case class Stop() extends Command

  private case class Recount() extends Command

  final case class Report(replyTo: ActorRef[SessionActor.Command]) extends Command

  trait ListOfRegisteredEntities

  final case class PooledEntities(label: String, length: Int, capacity: Int) extends ListOfRegisteredEntities

  final case class NumbersOfObjectTypes(label: String, length: Int) extends ListOfRegisteredEntities
}

class GuidBeanCounter(
                       context: ActorContext[GuidBeanCounter.Command],
                       zone: Zone,
                       numberSource: NumberPoolHub
                     )

  extends AbstractBehavior[GuidBeanCounter.Command](context) {

  implicit val log: Logger = org.log4s.getLogger(name = "UNS")
  var countingTimer: Cancellable = Default.Cancellable
  var tally: List[GuidBeanCounter.ListOfRegisteredEntities] = Nil

  override def onMessage(msg : GuidBeanCounter.Command) : Behavior[GuidBeanCounter.Command] = {
    import GuidBeanCounter._
    msg match {
      case Start() =>
        start()

      case Stop() =>
        stop()

      case Recount() =>
        tally = recount().toList

      case Report(replyTo) =>
        replyTo ! SessionActor.ObjectAllocationReport(zone, tally)
    }
    Behaviors.same
  }

  def start(): Unit = {
    countingTimer = new TypedActorContextOps(context).toClassic.system.scheduler.scheduleWithFixedDelay(
      initialDelay = 10 seconds,
      delay = 30 seconds,
      new TypedActorRefOps(context.self).toClassic,
      GuidBeanCounter.Recount()
    )
  }

  def stop(): Unit = {
    countingTimer.cancel()
  }

  def recount(): Iterable[GuidBeanCounter.ListOfRegisteredEntities] = {
    val list = numberSource.Numbers
      .collect {
        case number => numberSource.apply(number)
      }
      .collect {
        case Some(_ : LocalProjectile | _: LocalLockerItem) => "EXCLUDED"
        case Some(o : PlanetSideGameObject) => o.Definition.Name
        case Some(o) => o.getClass().getSimpleName()
      }
      .filterNot(_.equals("EXCLUDED"))
    val objectTypeCounts = list.toSet.map { entry: String =>
      val count = list.count(_.equals(entry))
      GuidBeanCounter.NumbersOfObjectTypes(entry, count)
    }
    val poolCounts = numberSource.Pools.iterator.map {
      case (name, pool) =>
        GuidBeanCounter.PooledEntities(name, pool.Count, pool.Numbers.length)
    }
    val corpsesCount = GuidBeanCounter.NumbersOfObjectTypes("corpses", zone.Corpses.length)
    val clutterCount = GuidBeanCounter.NumbersOfObjectTypes("debris", zone.EquipmentOnGround.length)
    (objectTypeCounts ++ poolCounts).toSeq :+ corpsesCount :+ clutterCount
  }
}
