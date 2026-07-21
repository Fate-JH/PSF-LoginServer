// Copyright (c) 2026 PSForever
package net.psforever.objects.definition.converter

import net.psforever.objects.serverobject.flag.module.VanuModule
import net.psforever.packet.game.objectcreate.VanuModuleCanisterData

import scala.util.{Success, Try}

class VanuModuleCanisterConverter extends ObjectCreateConverter[VanuModule]() {
  override def ConstructorData(obj: VanuModule): Try[VanuModuleCanisterData] = {
    Success(
      VanuModuleCanisterData(
        VanuModuleCanisterConverter.flag_converter.ConstructorData(obj).get,
        obj.ValidFlagType,
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
