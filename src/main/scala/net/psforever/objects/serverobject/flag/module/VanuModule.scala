// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.serverobject.flag.base.{CarriableFlag, FlagDefinition, OwnedFlag}
import net.psforever.types.PlanetSideEmpire

sealed abstract class ModuleChargeState(val value: Boolean)

object ModuleChargeState {
  case object NonPowered extends ModuleChargeState(value = false)
  case object Charging extends ModuleChargeState(value = false)
  case object Charged extends ModuleChargeState(value = true)
  case object Demo extends ModuleChargeState(value = true)
}

class VanuModule(fDef: FlagDefinition, validFlagType: CarriableFlag)
  extends OwnedFlag(fDef, validFlagType) {
  var start: Long = 0L
  var duration: Long = 1200000L //todo temporary

  private var charged: ModuleChargeState = ModuleChargeState.NonPowered

  def Charged: ModuleChargeState = charged

  def Charged_=(chargeState: ModuleChargeState): ModuleChargeState = {
    charged = chargeState
    Charged
  }

  def Charged_=(chargeState: Boolean): ModuleChargeState = {
    if (chargeState) {
      charged = ModuleChargeState.Charged
    } else {
      charged = ModuleChargeState.NonPowered
    }
    Charged
  }

  override def Faction: PlanetSideEmpire.Value = Carrier.map(_.Faction).getOrElse(super.Faction)

  override def DisplayFlagType: CarriableFlag = if (charged != ModuleChargeState.NonPowered) {
    ValidFlagType
  } else {
    CarriableFlag.VanuModuleNonPowered
  }
}
