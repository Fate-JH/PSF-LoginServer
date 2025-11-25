// Copyright (c) 2025 PSForever
package net.psforever.services.cluster

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{Behavior, SupervisorStrategy}
import net.psforever.objects.zones.{Zone, ZoneInfo}
import net.psforever.packet.game.StormData
import net.psforever.services.cluster.StormState.StormOverview
import net.psforever.services.cluster.Weather.Type
import net.psforever.services.local.{LocalAction, LocalServiceMessage}
import net.psforever.types.Vector3
import net.psforever.zones.Zones

object Weather {
  trait Type
  case object Clear extends Type
  case object Rain extends Type
  case object Snow extends Type
  case object Sandstorm extends Type
}

trait StormInfo {
  def location: Vector3
  def radius: Int
  def intensity: Int
}

sealed trait StormState extends StormInfo {
  def data: StormOverview
}

private[cluster] object StormState {
  final case class StormOverview(
                                  location: Vector3,
                                  radius: Int, //start radius, lower bound of radius
                                  maxRadius:Int,
                                  intensity: Int, //start intensity
                                  minIntensity: Int, //lower bound of intensity
                                  maxIntensity: Int, //upper bound of intensity
                                  growth: Int, //rate applied to intensity
                                  duration: Int, //minutes
                                  decay: Int //rate applied to intensity,
                                )

  final case class DevelopingStorm(
                                    data: StormOverview,
                                    location: Vector3,
                                    radius: Int,
                                    intensity: Int
                                  ) extends StormState

  final case class OngoingStorm(
                                 data: StormOverview,
                                 location: Vector3,
                                 radius: Int,
                                 intensity: Int,
                                 duration: Int
                               ) extends StormState

  final case class DecayingStorm(
                                  data: StormOverview,
                                  location: Vector3,
                                  radius: Int,
                                  intensity: Int
                                ) extends StormState
}


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

  final case class Start(
                          weatherType: Type,
                          zoneId: Int,
                          location: Vector3,
                          radius: Int,
                          maxRadius: Int,
                          intensity: Int,
                          minIntensity: Int,
                          maxIntensity: Int,
                          growthRate: Int,
                          timeAlive: Int,
                          decayRate: Int
                        ) extends Command

  final case class RandomStart(stormsToStart: Int) extends Command

  final case class Stop(weatherType: Weather.Type, zoneId: Int, location: Vector3) extends Command

  object Stop {
    def apply(zoneId: Int, location: Vector3): Stop = {
      Stop(Weather.Clear, zoneId, location)
    }
  }

  private case object Announce extends Command

  final case class ReportFor(zoneNumber: Int, replyTo: String) extends Command

  final case class WeatherConditions(index:Int, wtype: Weather.Type, windSpeed: Int, windDirection: Vector3)

  private[this] def RawStormsPerZone(list: List[StormInfo]): List[(LocalCoordinateResult, StormInfo)] = {
    list
      .flatMap { entry =>
        GlobalMap
          .GlobalCoordinatesToLocalMaps(entry.location.x, entry.location.y)
          .map { localCoords => (localCoords, entry) }
      }
  }

  private def StormsPerZone(list: List[StormInfo]): Map[Int, List[(LocalCoordinateResult, StormInfo)]] = {
    RawStormsPerZone(list)
      .groupBy(_._1.zoneNumber)
  }

  private def StormsInZone(zoneId: Int, list: List[StormInfo]): Map[Int, List[(LocalCoordinateResult, StormInfo)]] = {
    RawStormsPerZone(list)
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
      SendWeatherReport(zone, storms, toChannelOpt)
    }
  }

  private def SendWeatherReport(zone: Zone, storms: List[StormInfo], toChannelOpt: Option[String]): Unit = {
    val outChannel = toChannelOpt.getOrElse(zone.id)
    zone.LocalEvents ! LocalServiceMessage(outChannel,
      LocalAction.WeatherReport(
        zone.Number,
        Seq(),
        storms.map(storm => StormData(storm.location, storm.intensity, storm.radius)))
    )
  }
}

class WeatherService(
                      context: ActorContext[WeatherService.Command],
                      timer: TimerScheduler[WeatherService.Command],
                      _zones: Iterable[Zone])
  extends AbstractBehavior[WeatherService.Command](context) {
  import WeatherService._
  import StormState._
  //private[this] val log = org.log4s.getLogger("WeatherService")

  //private val radiusToLocalMapScale: Float = 8.192f

  private var weatherStartTest: Int = 10

  private val weatherConditionsInZone: List[WeatherConditions] =
    WeatherConditions(0, Weather.Clear, 0, Vector3.Zero) +:
    ZoneInfo.values.map { info => WeatherConditions(info.value, info.weather, 0, Vector3(0,1,0)) }.sortBy(_.index).toList

  private val stormRandomizer: scala.util.Random = new scala.util.Random

  private var activeStorms: List[StormState] = List[StormState]()

  override def onMessage(msg: WeatherService.Command): Behavior[WeatherService.Command] = {
    Behaviors
      .receiveMessage[WeatherService.Command] {
        case WeatherService.Announce =>
          //update all storm data
          //TODO radius actually works
          //TODO storms interact
          val (updatedStorms, reportedStorms): (List[Option[StormState]], List[StormInfo]) = {
            activeStorms.map {
              case stormInfo @ DevelopingStorm(data, location, radius, intensity) =>
                val changedStorm: StormState = if (radius < data.radius || intensity < data.intensity) {
                  stormInfo.copy(
                    radius = Math.min(radius + data.growth, data.radius),
                    intensity = Math.min(intensity + data.growth, data.intensity)
                  )
                } else {
                  OngoingStorm(data, location, radius, intensity, data.duration)
                }
                (Some(changedStorm), changedStorm)

              case stormInfo @ OngoingStorm(data, location, radius, intensity, duration) =>
                val windInfluence: Vector3 = CalculateWindInfluenceOnStorm(location)
                val changedStorm: StormState = if (duration == 0) {
                  DecayingStorm(data, location + windInfluence, radius, intensity)
                } else {
                  stormInfo.copy(
                    location = location + windInfluence,
                    radius = math.min(data.radius, math.max(data.maxRadius, radius + stormRandomizer.between(-3, 3))),
                    intensity = math.min(data.intensity, math.max(data.maxIntensity, intensity + stormRandomizer.between(-3, 3))),
                    duration = duration - 1
                  )
                }
                (Some(changedStorm), changedStorm)

              case stormInfo @ DecayingStorm(data, location, radius, intensity) =>
                val windInfluence: Vector3 = CalculateWindInfluenceOnStorm(location)
                val changedStorm: StormState = stormInfo.copy(
                  location = location + windInfluence,
                  radius = Math.max(radius - stormRandomizer.between(-1, 2), 0),
                  intensity = Math.max(intensity - data.decay + stormRandomizer.between(-1, 1), 0)
                )
                if (changedStorm.intensity > 0) {
                  (Some(changedStorm), changedStorm)
                } else {
                  (None, changedStorm)
                }

              case state =>
                (None, DecayingStorm(state.data, Vector3.Zero, 0, 0))
            }
          }.unzip
          //organize storm display data
          activeStorms = updatedStorms.flatten
          //dispatch messages
          SendWeatherReport(StormsPerZone(reportedStorms))
          //start new weather?
          PerformWeatherStartTest(zonesToSelect = 3)
          Behaviors.same

        case ReportFor(zoneNumber, replyTo) =>
          SendWeatherReport(StormsInZone(zoneNumber, activeStorms), Some(replyTo))
          Behaviors.same

        case WeatherService.Start(_, zoneId, location, radius, maxRadius, intensity, minIntensity, maxIntensity, growthRate, lifetime, decayRate) =>
          GlobalMap
            .LocalMapToGlobalCoordinates(zoneId, location.x, location.y)
            .foreach { coords =>
              val newStorm = DevelopingStorm(
                StormOverview(coords.toVector, radius, maxRadius, intensity, minIntensity, maxIntensity, growthRate, lifetime, decayRate),
                coords.toVector,
                0,
                0
              )
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

        case WeatherService.RandomStart(numberOfStormsToStart) =>
          val stormsBefore = activeStorms.size
          RunWeatherStartTest(numberOfStormsToStart)
          if (activeStorms.size > stormsBefore) {
            weatherStartTest = 10
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
                  val newStorm = DecayingStorm(globalInfo.data, globalInfo.location, globalInfo.radius, globalInfo.intensity)
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
    GlobalMap
      .GlobalCoordinatesToLocalMaps(location.x, location.y)
      .map { coords =>
        val conditions = weatherConditionsInZone
          .find(_.index == coords.zoneNumber)
          .getOrElse(weatherConditionsInZone.head)
        Vector3.Unit(conditions.windDirection) * conditions.windSpeed.toFloat
      }
      .foldLeft(Vector3.Zero)(_ + _) * 0.0021f
  }

  private def PerformWeatherStartTest(zonesToSelect: Int): Unit = {
    if (weatherStartTest == 0) {
      weatherStartTest = 10
      RunWeatherStartTest(zonesToSelect)
    } else {
      weatherStartTest -= 1
    }
  }

  private def RunWeatherStartTest(zonesToSelect: Int): Unit = {
    val newStorms: List[(Zone, StormState)] = stormRandomizer
      .shuffle(weatherConditionsInZone)
      .take(zonesToSelect)
      .filterNot(_.wtype == Weather.Clear)
      .flatMap { entry =>
        Zones.zones.find(_.Number == entry.index)
      }
      .filter { zone =>
        StormsInZone(zone.Number, activeStorms).isEmpty
      }
      .map { zone =>
        val resolvedCoordinates = GlobalMap.LocalMapToGlobalCoordinates(zone.Number, zone.map.scale.width * 0.5f, zone.map.scale.height * 0.5f).get
        val location = Vector3(resolvedCoordinates.x, resolvedCoordinates.y, 0f)
        (zone, DevelopingStorm(StormOverview(location, 217, 217, 240, 240, 240, 5, 55, 5), location, 0, 0))
      }
    if (newStorms.nonEmpty) {
      activeStorms = activeStorms ++ newStorms.map(_._2)
      newStorms.foreach { case (zone, allNewStormsInZone) =>
        activeStorms = activeStorms :+ allNewStormsInZone
        WeatherService.SendWeatherReport(zone, List(allNewStormsInZone), None)
      }
    }
  }
}
