// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.serverobject.flag.base.{FlagDefinition, CarriableFlag, OwnedFlag}

sealed abstract class ChargedState(val value: Boolean)

object ChargedState {
  case object NonPowered extends ChargedState(value = false)
  case object Charged extends ChargedState(value = true)
  case object Demo extends ChargedState(value = true)
}

class VanuModule(fDef: FlagDefinition, validFlagType: CarriableFlag)
  extends OwnedFlag(fDef, validFlagType) {
  val duration: Long = 1200000L //todo temporary

  private var charged: ChargedState = ChargedState.NonPowered

  def Charged: ChargedState = charged

  def Charged_=(chargeState: ChargedState): ChargedState = {
    charged = chargeState
    Charged
  }

  def Charged_=(chargeState: Boolean): ChargedState = {
    if (chargeState) {
      charged = ChargedState.Charged
    } else {
      charged = ChargedState.NonPowered
    }
    Charged
  }

  override def DisplayFlagType: CarriableFlag = if (charged != ChargedState.NonPowered) {
    ValidFlagType
  } else {
    CarriableFlag.VanuModuleNonPowered
  }
}
