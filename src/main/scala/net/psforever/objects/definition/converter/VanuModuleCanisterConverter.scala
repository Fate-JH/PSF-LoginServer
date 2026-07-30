// Copyright (c) 2026 PSForever
package net.psforever.objects.definition.converter

import net.psforever.objects.Default
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.objects.serverobject.structures.Building
import net.psforever.packet.game.objectcreate.{CaptureFlagData, FlagTypeData, PlacementData, VanuModuleCanisterData}
import net.psforever.types.PlanetSideEmpire

import scala.util.{Success, Try}

class VanuModuleCanisterConverter extends ObjectCreateConverter[VanuModule]() {
  override def ConstructorData(obj: VanuModule): Try[VanuModuleCanisterData] = {
    val time = math.min(0L, obj.duration - (System.currentTimeMillis() - obj.InitialSpawnTime))
    Success(
      VanuModuleCanisterData(
        CaptureFlagData(
          PlacementData(obj.Position, obj.Orientation, None),
          PlanetSideEmpire.VS,
          Default.GUID0.guid,
          Default.GUID0.guid,
          120000L
        ),
        FlagTypeData.Vehicle, //obj.ValidFlagType,
        unk1 = 0,
        unk2 = false,
        unk3 = 0,
        unk4 = 0
      )
    )
  }
}

object VanuModuleCanisterConverter {
  private lazy val flag_converter = new CaptureFlagConverter()
}
