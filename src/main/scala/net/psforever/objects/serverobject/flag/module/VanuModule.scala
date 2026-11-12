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
  case class Expired(override val value: Boolean) extends ModuleChargeState(value)
}

class VanuModule(fDef: FlagDefinition, validFlagType: CarriableFlag)
  extends OwnedFlag(fDef, validFlagType) {
  private var end: Long = Long.MaxValue
  private var expirationEnd: Long = Long.MaxValue

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

  def FlagFaction: PlanetSideEmpire.Value = super.Faction

  override def Faction: PlanetSideEmpire.Value = Carrier.map(_.Faction).getOrElse(super.Faction)

  override def DisplayFlagType: CarriableFlag = {
    if (Destroyed) {
      CarriableFlag.VanuModuleInvalid
    } else if (charged.value) {
      ValidFlagType
    } else {
      CarriableFlag.VanuModuleNonPowered
    }
  }

  def SetTime(durationTime: Long): Unit = {
    SetTime(durationTime, System.currentTimeMillis())
  }

  def SetTime(durationTime: Long, startTime: Long = System.currentTimeMillis()): Unit = {
    try {
      end = startTime + durationTime
    } catch {
      case _: Exception =>
        end = Long.MaxValue
    }
  }

  def isDurationSet: Boolean = {
    end < Long.MaxValue
  }

  def GetRemainingTimeAlive(): Long = {
    GetRemainingTimeAlive(defaultTime = Long.MaxValue)
  }

  def GetRemainingTimeAlive(defaultTime: Long): Long = {
    if (Destroyed) {
      0L
    } else if (isDurationSet) {
      math.max(end - System.currentTimeMillis(), 0L)
    } else {
      defaultTime
    }
  }

  def SetExpirationTime(durationTime: Long): Unit = {
    SetExpirationTime(durationTime, System.currentTimeMillis())
  }

  def SetExpirationTime(durationTime: Long, startTime: Long): Unit = {
    try {
      expirationEnd = startTime + durationTime
    } catch {
      case _: Exception =>
        expirationEnd = Long.MaxValue
    }
  }

  def isExpirationDurationSet: Boolean = {
    expirationEnd < Long.MaxValue
  }

  def GetRemainingExpirationTime(): Long = {
    GetRemainingExpirationTime(System.currentTimeMillis(), defaultTime = Long.MaxValue)
  }

  def GetRemainingExpirationTime(defaultTime: Long): Long = {
    GetRemainingExpirationTime(System.currentTimeMillis(), defaultTime)
  }

  def GetRemainingExpirationTime(curr: Long, defaultTime: Long): Long = {
    if (Destroyed) {
      0L
    } else if (isExpirationDurationSet) {
      math.max(expirationEnd - curr, 0L)
    } else {
      defaultTime
    }
  }
}
