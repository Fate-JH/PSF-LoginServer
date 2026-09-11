// Copyright (c) 2026 PSForever
package net.psforever.objects.definition.converter

import net.psforever.objects.Default
import net.psforever.objects.serverobject.flag.base.CarriableModule
import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.packet.game.objectcreate.{CommonFieldData, CommonFieldDataWithPlacement, PlacementData, VanuModuleCanisterData}
import net.psforever.types.{PlanetSideEmpire, VanuModuleType}

import scala.util.{Success, Try}

class VanuModuleCanisterConverter extends ObjectCreateConverter[VanuModule]() {
  override def ConstructorData(obj: VanuModule): Try[VanuModuleCanisterData] = {
    Success(
      VanuModuleCanisterData(
        CommonFieldDataWithPlacement(
          PlacementData(obj.Position, obj.Orientation, None),
          CommonFieldData(
            PlanetSideEmpire.NEUTRAL,
            bops = false,
            alternate = obj.Destroyed,
            v1 = false,
            v2 = None,
            jammered = false,
            v5 = None,
            guid = Default.GUID0
          )
        ),
        unk = 0L,
        obj.ValidFlagType match {
          case cmod: CarriableModule => cmod.module
          case _ => VanuModuleType.Invalid
        }
      )
    )
  }
}
