// Copyright (c) 2025 PSForever
package net.psforever.services.cluster

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{Behavior, SupervisorStrategy}
import net.psforever.objects.zones.{Zone, ZoneInfo}
import net.psforever.packet.game.StormData
import net.psforever.services.cluster.Weather.Type
import net.psforever.services.local.{LocalAction, LocalServiceMessage}
import net.psforever.types.Vector3
import net.psforever.zones.Zones

trait StormInfo {
  def location: Vector3
  def radius: Int
  def intensity: Int
}

trait StormState extends StormInfo {
  def prior: StormState = this
}

private case class StandaloneStormState(
                                       location: Vector3,
                                       radius: Int,
                                       intensity: Int
                                     ) extends StormState

final case class DevelopingStorm(
                                  override val prior: StormState,
                                  location: Vector3,
                                  radius: Int,
                                  intensity: Int
                                ) extends StormState

object DevelopingStorm {
  def apply(location: Vector3, radius: Int, intensity: Int): DevelopingStorm = {
    DevelopingStorm(StandaloneStormState(location, radius, intensity), location, 0, 0)
  }
}

final case class RagingStorm(
                             override val prior: StormState,
                             location: Vector3,
                             radius: Int,
                             intensity: Int,
                             duration: Int
                           ) extends StormState

final case class DecayingStorm(
                               override val prior: StormState,
                               location: Vector3,
                               radius: Int,
                               intensity: Int
                             ) extends StormState

object WeatherService {
  val WeatherServiceKey: ServiceKey[Command] = ServiceKey[WeatherService.Command]("WeatherControl")

  def apply(zones: Iterable[Zone]): Behavior[Command] =
    Behaviors
      .supervise[Command] {
          Behaviors.setup { context =>
            Behaviors.withTimers { timer =>
              import scala.concurrent.duration._
              context.system.receptionist ! Receptionist.Register(WeatherServiceKey, context.self)
              timer.startTimerAtFixedRate(WeatherService.Announce, 1.minute)
              new WeatherService(context, timer, zones)
            }
          }
      }
      .onFailure[Exception](SupervisorStrategy.restart)

  sealed trait Command

  final case class Start(weatherType: Type, zoneId: Int, location: Vector3, radius: Int, intensity: Int) extends Command

  final case class Stop(weatherTypes: Seq[Weather.Type], zoneId: Int, location: Vector3) extends Command

  object Stop {
    def apply(zoneId: Int, location: Vector3): Stop = {
      Stop(Seq(), zoneId, location)
    }

    def apply(weatherType: Weather.Type): Stop = {
      Stop(Seq(weatherType), -1)
    }

    def apply(weatherTypes: Seq[Weather.Type], zoneId: Int): Stop = {
      Stop(weatherTypes, zoneId, Vector3.Zero.copy(z = -1f))
    }
  }

  private case object Announce extends Command

  final case class ReportFor(zoneNumber: Int, replyTo: String) extends Command

  final case class WeatherConditions(index:Int, windSpeed: Int, windDirection: Vector3)

  private def NoWeatherConditions(index: Int): WeatherConditions = {
    WeatherConditions(index, 0, Vector3.Zero)
  }

  private def StormsPerZone(list: List[StormInfo]): Map[Int, List[(LocalCoordinateResult, StormInfo)]] = {
    list
      .flatMap { entry =>
        GlobalMap
          .GlobalCoordinatesToLocalMaps(entry.location.x, entry.location.y)
          .map { localCoords => (localCoords, entry) }
      }
      .groupBy(_._1.zoneNumber)
  }

  private def StormsInZone(zoneId: Int, list: List[StormInfo]): Map[Int, List[(LocalCoordinateResult, StormInfo)]] = {
    list
      .flatMap { entry =>
        GlobalMap
          .GlobalCoordinatesToLocalMaps(entry.location.x, entry.location.y)
          .map { localCoords => (localCoords, entry) }
      }
      .filter(_._1.zoneNumber == zoneId)
      .groupBy(_._1.zoneNumber)
  }

  private def SendWeatherReport(stormsPerZone: Map[Int, List[(LocalCoordinateResult, StormInfo)]], toChannelOpt: Option[String] = None): Unit = {
    stormsPerZone
      .foreach { case (index, info) =>
        val storms = info.map(_._2)
        SendWeatherReport(index, storms, toChannelOpt)
      }
  }

  private def SendWeatherReport(zoneId: Int, storms: List[StormInfo], toChannelOpt: Option[String]): Unit = {
    Zones.zones.find(_.Number == zoneId).foreach { zone =>
      val outChannel = toChannelOpt.getOrElse(zone.id)
      zone.LocalEvents ! LocalServiceMessage(outChannel,
        LocalAction.WeatherReport(
          zoneId,
          Seq(),
          storms.map(storm => StormData(storm.location, storm.intensity, storm.radius)))
      )
    }
  }
}

class WeatherService(
                      context: ActorContext[WeatherService.Command],
                      timer: TimerScheduler[WeatherService.Command],
                      _zones: Iterable[Zone])
  extends AbstractBehavior[WeatherService.Command](context) {
  import WeatherService._
  //private[this] val log = org.log4s.getLogger("WeatherService")

  private val weatherConditionsInZone: List[WeatherConditions] = List(
    NoWeatherConditions(index = 0), //0.nowhere
    WeatherConditions(1, 0, Vector3(0,1,0)), //1.solsar
    WeatherConditions(2, 0, Vector3(0,1,0)), //2.hossin
    WeatherConditions(3, 0, Vector3(0,1,0)), //3.cyssor
    WeatherConditions(4, 0, Vector3(0,1,0)), //4.ishundar
    WeatherConditions(5, 0, Vector3(0,1,0)), //5.forseral
    WeatherConditions(6, 0, Vector3(0,1,0)), //6.ceryshen
    WeatherConditions(7, 0, Vector3(0,1,0)), //7.esamir
    WeatherConditions(8, 0, Vector3(0,1,0)), //8.oshur
    WeatherConditions(9, 0, Vector3(0,1,0)), //9.searhus
    WeatherConditions(10, 0, Vector3(0,1,0)), //10.amerish
    WeatherConditions(11, 0, Vector3(0,1,0)), //11.nc
    WeatherConditions(12, 0, Vector3(0,1,0)), //12.tr
    WeatherConditions(13, 0, Vector3(0,1,0)), //13.vs
    NoWeatherConditions(index = 14), //14.tr, shooting
    NoWeatherConditions(index = 15), //15.tr, driving
    NoWeatherConditions(index = 16), //16.tr, co-op
    NoWeatherConditions(index = 17), //17.nc, shooting
    NoWeatherConditions(index = 18), //18.nc, driving
    NoWeatherConditions(index = 19), //19.nc, co-op
    NoWeatherConditions(index = 20), //20.vs, shooting
    NoWeatherConditions(index = 21), //21.vs, driving
    NoWeatherConditions(index = 22), //22.vs, co-op
    NoWeatherConditions(index = 23), //23.supai
    NoWeatherConditions(index = 24), //24.hunhau
    NoWeatherConditions(index = 25), //25.adlivun
    NoWeatherConditions(index = 26), //26.byblos
    NoWeatherConditions(index = 27), //27.annwn
    NoWeatherConditions(index = 28), //28.drugaskan
    NoWeatherConditions(index = 29), //29.bi, extinction
    NoWeatherConditions(index = 30), //30.bi, ascension
    NoWeatherConditions(index = 31), //31.bi, desolation
    NoWeatherConditions(index = 32)  //32.bi, nexus
  )

  private var activeStorms: List[StormState] = GlobalMap.Details
    .filter(_.corner != GlobalMap.ZeroCoordinates)
    .flatMap { entry =>
      ZoneInfo
        .values
        .find(_.value == entry.index)
        .map { zone =>
          val resolvedCoordinates = GlobalMap.LocalMapToGlobalCoordinates(entry.index, zone.map.scale.width * 0.5f, zone.map.scale.height * 0.5f).get
          val location = Vector3(resolvedCoordinates.x, resolvedCoordinates.y, 0f)
          RagingStorm(StandaloneStormState(location, 217, 240), location, 217, 240, 5)
        }
    }

  override def onMessage(msg: WeatherService.Command): Behavior[WeatherService.Command] = {
    Behaviors
      .receiveMessage[WeatherService.Command] {
        case WeatherService.Announce =>
          //update all storm data
          //TODO radius actually works
          //TODO storms interact
          val (updatedStorms, reportedStorms): (List[Option[StormState]], List[StormInfo]) = {
            activeStorms.map {
              case stormInfo @ DevelopingStorm(prior, location, radius, intensity) =>
                val changedStorm: StormState = if (radius < prior.radius || intensity < prior.intensity) {stormInfo.copy(
                    radius = prior.radius,
                    intensity = Math.min(intensity + 5, prior.intensity)
                  )
                } else {
                  RagingStorm(stormInfo, location, radius, intensity, 30)
                }
                (Some(changedStorm), changedStorm)

              case stormInfo @ RagingStorm(_, location, radius, intensity, duration) =>
                val windInfluence: Vector3 = CalculateWindInfluenceOnStorm(location)
                val changedStorm: StormState = if (duration == 0) {
                  DecayingStorm(stormInfo, location + windInfluence, radius, Math.max(intensity - 5, 0))
                } else {
                  stormInfo.copy(
                    location = location + windInfluence,
                    duration = duration - 1
                  )
                }
                (Some(changedStorm), changedStorm)

              case stormInfo @ DecayingStorm(_, location, _, intensity) =>
                val windInfluence: Vector3 = CalculateWindInfluenceOnStorm(location)
                val changedStorm: StormState = stormInfo.copy(
                  location = location + windInfluence,
                  //radius = Math.max(radius - 5, 0),
                  intensity = Math.max(intensity - 5, 0)
                )
                if (changedStorm.intensity > 0) {
                  (Some(changedStorm), changedStorm)
                } else {
                  (None, changedStorm)
                }
            }
          }.unzip
          //organize storm display data
          activeStorms = updatedStorms.flatten
          //dispatch messages
          SendWeatherReport(StormsPerZone(reportedStorms))
          Behaviors.same

        case ReportFor(zoneNumber, replyTo) =>
          SendWeatherReport(StormsInZone(zoneNumber, activeStorms), Some(replyTo))
          Behaviors.same

        case WeatherService.Start(_, zoneId, location, radius, intensity) =>
          GlobalMap
            .LocalMapToGlobalCoordinates(zoneId, location.x, location.y)
            .foreach { coords =>
              val newStorm = DevelopingStorm(coords.toVector, radius, intensity)
              val stormsInZone = StormsInZone(zoneId, activeStorms)
              if (!stormsInZone(zoneId).exists { case (localCoords, _) =>
                Vector3.DistanceSquared(localCoords.toVector, location) < 168100f
              }) {
                activeStorms = (activeStorms :+ newStorm).sortBy(_.location.x)
                SendWeatherReport(
                  stormsInZone.updated(
                    zoneId,
                    stormsInZone(zoneId) :+ (LocalCoordinateResult(zoneId, location.x, location.y), newStorm)
                  )
                )
              }
            }
          Behaviors.same

        case WeatherService.Stop(_, zoneId, location) =>
          val (stormsInThisZone, allStorms): (List[Option[StormInfo]], List[StormState]) = {
            activeStorms.map {
              globalInfo =>
                val localInfo = GlobalMap.GlobalCoordinatesToLocalMaps(globalInfo.location.x, globalInfo.location.y)
                if (!localInfo.exists(_.zoneNumber == zoneId)) {
                  (None, globalInfo)
                } else if (localInfo.exists { localCoords => Vector3.DistanceSquared(localCoords.toVector, location) < 168100f}) {
                  val newStorm = DecayingStorm(globalInfo, globalInfo.location, globalInfo.radius, globalInfo.intensity)
                  (Some(newStorm), newStorm)
                } else {
                  (Some(globalInfo), globalInfo)
                }
            }
          }.unzip
          activeStorms = allStorms
          SendWeatherReport(zoneId, stormsInThisZone.flatten, None)
          Behaviors.same
      }
  }
  
  private def CalculateWindInfluenceOnStorm(location: Vector3): Vector3 = {
    Vector3.Unit(
      GlobalMap
        .GlobalCoordinatesToLocalMaps(location.x, location.y)
        .map { coords =>
          val conditions = weatherConditionsInZone
            .find(_.index == coords.zoneNumber)
            .getOrElse(weatherConditionsInZone.head)
          conditions.windDirection * conditions.windSpeed.toFloat
        }
        .foldLeft(Vector3.Zero)(_ + _)
    ) * 0.0025f
  }
}

object Weather {
  trait Type

  object Default extends Type

  object Clear extends Type

  object Rain extends Type

  object Snow extends Type

  object Sandstorm extends Type
}
