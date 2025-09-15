// Copyright (c) 2025 PSForever
package net.psforever.services.cluster

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{Behavior, SupervisorStrategy}
import net.psforever.objects.zones.{Zone, ZoneInfo}
import net.psforever.packet.game.StormData
import net.psforever.services.cluster.Weather.Type
import net.psforever.services.local.{LocalAction, LocalServiceMessage}
import net.psforever.types.Vector3
import net.psforever.zones.Zones

final case class StormInfo(location: Vector3, radius: Int, intensity: Int)

object WeatherService {
  val InterstellarClusterServiceKey: ServiceKey[Command] =
    ServiceKey[WeatherService.Command]("WeatherControl")

  def apply(zones: Iterable[Zone]): Behavior[Command] =
    Behaviors
      .supervise[Command] {
          Behaviors.setup { context =>
            context.system.receptionist ! Receptionist.Register(InterstellarClusterServiceKey, context.self)
            val o = new WeatherService(context, zones)
            o.onMessage(InitTimer)
            o
          }
      }
      .onFailure[Exception](SupervisorStrategy.restart)

  sealed trait Command

  private case object InitTimer extends Command

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

  private case class Coordinates(x: Float, y: Float)
  private case class CoordinateCompare(px: Int, py: Int, x: Float, y: Float)

  private final object NoSize extends CoordinateCompare(px = 0, py = 0, math.ulp(1.0f), math.ulp(1.0f)) //non-zero to avoid diide by zero errors
  private final object Small extends CoordinateCompare(px = 198, py = 198, 0.220000f, 0.304615f)
  private final object Large extends CoordinateCompare(px = 200, py = 200, 0.222222f, 0.307692f)

  private val ZeroCoordinates: CoordinateCompare = CoordinateCompare(px = 0, py = 0, 0.0f, 0.0f)

  private val GlobalMap: Coordinates = Coordinates(900, 650)

  private case class ZonesOnMap(index: Int, corner: CoordinateCompare, scale: CoordinateCompare) {
    lazy val (scaleModifierWidth, scaleModifierHeight): (Float, Float) = {
      Zones.zones.find(_.Number == index) match {
        case Some(zone) =>
          (scale.x / zone.map.scale.width, scale.y / zone.map.scale.height)
        case None =>
          (0f, 0f)
      }
    }
  }

  private def NoZone(index: Int): ZonesOnMap = {
    ZonesOnMap(index, ZeroCoordinates, NoSize)
  }

  private val mapList: List[ZonesOnMap] = List(
    NoZone(index = 0), //0.nowhere
    ZonesOnMap(1, CoordinateCompare(169, 166, 0.187777f, 0.255384f), Large), //1.solsar
    ZonesOnMap(2, CoordinateCompare(707, 213, 0.785555f, 0.327692f), Large), //2.hossin
    ZonesOnMap(3, CoordinateCompare(8, 11, 0.008888f, 0.016923f), Large), //3.cyssor
    ZonesOnMap(4, CoordinateCompare(333, 453, 0.370000f, 0.696923f), Large), //4.ishundar
    ZonesOnMap(5, CoordinateCompare(546, 331, 0.606666f, 0.509230f), Large), //5.forseral
    ZonesOnMap(6, CoordinateCompare(-10, 172, -0.011111f, 0.264615f), Large), //6.ceryshen
    ZonesOnMap(7, CoordinateCompare(690, 13, 0.766666f, 0.037142f), Large), //7.esamir
    ZonesOnMap(8, CoordinateCompare(422, 35, 0.468888f, 0.053846f), Large), //8.oshur
    ZonesOnMap(9, CoordinateCompare(351, 235, 0.390000f, 0.361538f), Large), //9.searhus
    ZonesOnMap(10, CoordinateCompare(26, 276, 0.028888f, 0.424615f), Large), //10.amerish
    ZonesOnMap(11, CoordinateCompare(306, -17, 0.340000f, -0.026153f), Small), //11.nc
    ZonesOnMap(12, CoordinateCompare(200, 370, 0.222222f, 0.569230f), Small), //12.tr
    ZonesOnMap(13, CoordinateCompare(521, 152, 0.578888f, 0.233846f), Small), //13.vs
    NoZone(index = 14), //14.tr, shooting
    NoZone(index = 15), //15.tr, driving
    NoZone(index = 16), //16.tr, co-op
    NoZone(index = 17), //17.nc, shooting
    NoZone(index = 18), //18.nc, driving
    NoZone(index = 19), //19.nc, co-op
    NoZone(index = 20), //20.vs, shooting
    NoZone(index = 21), //21.vs, driving
    NoZone(index = 22), //22.vs, co-op
    NoZone(index = 23), //23.supai
    NoZone(index = 24), //24.hunhau
    NoZone(index = 25), //25.adlivun
    NoZone(index = 26), //26.byblos
    NoZone(index = 27), //27.annwn
    NoZone(index = 28), //28.drugaskan
    NoZone(index = 29), //29.bi, extinction
    NoZone(index = 30), //30.bi, ascension
    NoZone(index = 31), //31.bi, desolation
    NoZone(index = 32)  //32.bi, nexus
  )

  final case class GlobalCoordinateResult(x: Float, y: Float)

  def LocalMapToGlobalCoordinates(mapIndex: Int, localX: Float, localY: Float): Option[GlobalCoordinateResult] = {
    mapList.lift(mapIndex) match {
      case Some(entry) if entry.scale == NoSize =>
        None
      case Some(entry) =>
        val globalPositionX = entry.corner.x + localX * entry.scaleModifierWidth
        val globalPositionY = entry.corner.y + localY * entry.scaleModifierHeight
        Some(GlobalCoordinateResult(globalPositionX, globalPositionY))
      case _ =>
        None
    }
  }

  final case class LocalCoordinateResult(zoneNumber: Int, x: Float, y: Float)

  def GlobalCoordinatesToLocalMaps(globalX: Float, globalY: Float): List[LocalCoordinateResult] = {
    mapList
      .filter { entry =>
        entry.corner.x <= globalX && globalX <= entry.corner.x + entry.scale.x &&
          entry.corner.y <= globalY && globalY <= entry.corner.y + entry.scale.y
      }
      .map { entry =>
        val zone = Zones.zones.find(_.Number == entry.index).get
        val outx = ((globalX - entry.corner.x) * zone.map.scale.width) / entry.scale.x
        val outy = ((globalY - entry.corner.y) * zone.map.scale.height) / entry.scale.y
        LocalCoordinateResult(entry.index, outx, outy)
      }
  }
}

class WeatherService(context: ActorContext[WeatherService.Command], _zones: Iterable[Zone])
  extends AbstractBehavior[WeatherService.Command](context) {
  import WeatherService._
  //private[this] val log = org.log4s.getLogger(self.path.name)

  private var activeStorms: List[StormInfo] = mapList
    .filter(_.corner != WeatherService.ZeroCoordinates)
    .flatMap { entry =>
      ZoneInfo
        .values
        .find(_.value == entry.index)
        .map { zone =>
          val resolvedCoordinates = WeatherService.LocalMapToGlobalCoordinates(entry.index, zone.map.scale.width * 0.5f, zone.map.scale.height * 0.5f).get
          StormInfo(Vector3(resolvedCoordinates.x, resolvedCoordinates.y, 0f), 217, 240)
        }
    }

  override def onMessage(msg: WeatherService.Command): Behavior[WeatherService.Command] = {
    Behaviors.withTimers { timer =>
      Behaviors
        .receiveMessage[WeatherService.Command] {
          case WeatherService.InitTimer =>
            if (activeStorms.nonEmpty) {
              import scala.concurrent.duration._
              timer.startTimerAtFixedRate(WeatherService.Announce, 1.minute)
            }
            Behaviors.same

          case WeatherService.Announce =>
            //organize storm display data
            //TODO determine how radius works
            val stormsPerZone = activeStorms
              .flatMap { entry =>
                WeatherService
                  .GlobalCoordinatesToLocalMaps(entry.location.x, entry.location.y)
                  .map { localCoords => (localCoords, entry) }
              }
              .groupBy(_._1.zoneNumber)
            SendWeatherReport(stormsPerZone)
            Behaviors.same

          case ReportFor(zoneNumber, replyTo) =>
            val stormsPerZone = activeStorms
              .flatMap { entry =>
                WeatherService
                  .GlobalCoordinatesToLocalMaps(entry.location.x, entry.location.y)
                  .map { localCoords => (localCoords, entry) }
              }
              .filter(_._1.zoneNumber == zoneNumber)
              .groupBy(_._1.zoneNumber)
            //dispatch messages
            SendWeatherReport(stormsPerZone, Some(replyTo))
            Behaviors.same

          case WeatherService.Start(weatherType, zoneId, location, radius, intensity) =>
            Behaviors.same

          case WeatherService.Stop(weatherTypes, zoneId, location) =>
            Behaviors.same
        }
    }
  }

  private def SendWeatherReport(stormsPerZone: Map[Int, List[(LocalCoordinateResult, StormInfo)]], toChannelOpt: Option[String] = None): Unit = {
    stormsPerZone
      .foreach { case (index, info) =>
        val storms = info.map(_._2)
        Zones.zones.find(_.Number == index).foreach { zone =>
          val outChannel = toChannelOpt.getOrElse(zone.id)
          zone.LocalEvents ! LocalServiceMessage(outChannel,
            LocalAction.WeatherReport(
              index,
              Seq(),
              storms.map(storm => StormData(storm.location, storm.intensity, storm.radius)))
          )
        }
      }
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
