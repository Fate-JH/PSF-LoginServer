// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.serverobject.flag.base.{FlagDefinition, CarriableFlag, OwnedFlag}

class VanuModule(fDef: FlagDefinition, validFlagType: CarriableFlag)
  extends OwnedFlag(fDef, validFlagType) {
  val duration: Long = 1200000L //todo temporary

  private var charged: Boolean = false

  def Charged: Boolean = charged

  def Charged_=(chargeState: Boolean): Boolean = {
    charged = chargeState
    Charged
  }

  override def ValidFlagType: CarriableFlag = if (charged) {
    validFlagType
  } else {
    CarriableFlag.VanuModuleNonPowered
  }
}
