// Copyright (c) 2025 PSForever
package net.psforever.services.cluster

import net.psforever.objects.zones.ZoneInfo
import net.psforever.types.Vector3
import net.psforever.zones.Zones

trait CoordinateConversionToVector {
  def x: Float
  def y: Float
  def toVector: Vector3 = Vector3(x, y, 0)
}

final case class GlobalCoordinateResult(x: Float, y: Float) extends CoordinateConversionToVector

final case class LocalCoordinateResult(zoneNumber: Int, x: Float, y: Float) extends CoordinateConversionToVector

object GlobalMap {
  final case class Coordinates(x: Float, y: Float)
  /**
   * px and py are pixel distances on the global map
   * x and y are float distances corresponding to those pixel distances
   */
  case class CoordinateCompare(px: Int, py: Int, x: Float, y: Float)

  private final object NoSize extends CoordinateCompare(px = 0, py = 0, math.ulp(1.0f), math.ulp(1.0f)) //non-zero to avoid divide by zero errors
  private final object Small extends CoordinateCompare(px = 198, py = 198, 0.220000f, 0.304615f)
  private final object Large extends CoordinateCompare(px = 200, py = 200, 0.222222f, 0.307692f)

  final val ZeroCoordinates: CoordinateCompare = CoordinateCompare(px = 0, py = 0, 0.0f, 0.0f)

  final val scale: Coordinates = Coordinates(900, 650)

  final case class ZonesOnMap(
                               index: Int,
                               corner: CoordinateCompare,
                               scale: CoordinateCompare
                             ) {
    lazy val (_, scaleModifierWidth, scaleModifierHeight): (ZoneInfo, Float, Float) = {
      ZoneInfo.values.find(_.value == index) match {
        case Some(zone) =>
          (zone, scale.x / zone.map.scale.width, scale.y / zone.map.scale.height)
        case None =>
          throw new IllegalArgumentException("no zone found with that index")
      }
    }
  }

  def NoZone(index: Int): ZonesOnMap = {
    ZonesOnMap(index, ZeroCoordinates, NoSize)
  }

  private[cluster] val Details: List[ZonesOnMap] = List(
    NoZone(index = 0), //0.nowhere
    ZonesOnMap(1, CoordinateCompare(168, 485, 0.186666f, 0.746153f), Large), //1.solsar
    ZonesOnMap(2, CoordinateCompare(706, 439, 0.784444f, 0.675384f), Large), //2.hossin
    ZonesOnMap(3, CoordinateCompare(7, 641, 0.007777f, 0.986153f), Large), //3.cyssor
    ZonesOnMap(4, CoordinateCompare(332, 200, 0.368888f, 0.307692f), Large), //4.ishundar
    ZonesOnMap(5, CoordinateCompare(545, 332, 0.605555f, 0.495384f), Large), //5.forseral
    ZonesOnMap(6, CoordinateCompare(-9, 180, -0.010000f, 0.276923f), Large), //6.ceryshen
    ZonesOnMap(7, CoordinateCompare(689, 639, 0.765555f, 0.983076f), Large), //7.esamir
    ZonesOnMap(8, CoordinateCompare(421, 617, 0.466666f, 0.949230f), Large), //8.oshur
    ZonesOnMap(9, CoordinateCompare(350, 417, 0.388888f, 0.641538f), Large), //9.searhus
    ZonesOnMap(10, CoordinateCompare(25, 376, 0.027777f, 0.578461f), Large), //10.amerish
    ZonesOnMap(11, CoordinateCompare(255, 684, 0.283333f, 1.052307f), Small), //11.nc
    ZonesOnMap(12, CoordinateCompare(199, 281, 0.221111f, 0.432307f), Small), //12.tr
    ZonesOnMap(13, CoordinateCompare(520, 499, 0.577777f, 0.767692f), Small), //13.vs
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

  def LocalMapToGlobalCoordinates(mapIndex: Int, localX: Float, localY: Float): Option[GlobalCoordinateResult] = {
    Details.lift(mapIndex) match {
      case Some(entry) if entry.scale == NoSize =>
        None
      case Some(entry) =>
        val globalPositionX = entry.corner.x + localX * entry.scaleModifierWidth
        val globalPositionY = entry.corner.y - localY * entry.scaleModifierHeight
        Some(GlobalCoordinateResult(globalPositionX, globalPositionY))
      case _ =>
        None
    }
  }

  def GlobalCoordinatesToLocalMaps(globalX: Float, globalY: Float): List[LocalCoordinateResult] = {
    Details
      .filter { entry =>
        entry.corner.x <= globalX && globalX <= entry.corner.x + entry.scale.x &&
          globalY <= entry.corner.y && globalY >= entry.corner.y - entry.scale.y
      }
      .map { entry =>
        val zone = Zones.zones.find(_.Number == entry.index).get
        val outx = ((globalX - entry.corner.x) * zone.map.scale.width) / entry.scale.x
        val outy = ((entry.corner.y - globalY) * zone.map.scale.height) / entry.scale.y
        LocalCoordinateResult(entry.index, outx, outy)
      }
  }
}
