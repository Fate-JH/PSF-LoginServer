// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import net.psforever.types.{CavernBenefit, VanuModuleType}

/* All flag categories */

sealed abstract class FlagCategory(val value: String)

object FlagCategory {
  case object CaptureFlag extends FlagCategory(value = "CaptureFlag")
  case object VanuModule extends FlagCategory(value = "VanuModule")
  /* Related to Vanu modules, but is not a vanu module */
  case object VanuModuleElement extends FlagCategory(value = "VanuModuleElement")
  case object MonolithUnit extends FlagCategory(value = "MonolithUnit")
  case object RabbitBall extends FlagCategory(value = "RabbitBall")
}

/* All flags and vanu modules specifically */

sealed abstract class CarriableFlag(val value: String, val category: FlagCategory)

/** Special enhancement modules generated in cavern facilities to be installed into above ground facilities. */
abstract class CarriableModule(override val value: String, val module: VanuModuleType, val benefit: CavernBenefit)
  extends CarriableFlag(value, FlagCategory.VanuModule)

object CarriableFlag {
  /** The lattice logic unit (LLU) */
  case object CaptureFlag extends CarriableFlag(value = FlagCategory.CaptureFlag.value, FlagCategory.CaptureFlag)

  /** Not really a module, rather the cavity in which a module is stored */
  case object VanuModuleCradle extends CarriableFlag(value = "VanuModuleCradle", FlagCategory.VanuModuleElement)
  /** Speed module (orange) */
  case object VanuModuleSpeed extends CarriableModule(value = "VanuModuleSpeed", VanuModuleType.Speed, CavernBenefit.SpeedModule)
  /** Shield module (aqua) */
  case object VanuModuleDefender extends CarriableModule(value = "VanuModuleDefender", VanuModuleType.Defender, CavernBenefit.ShieldModule)
  /** Vehicle module (purple) */
  case object VanuModuleVehicle extends CarriableModule(value = "VanuModuleVehicle", VanuModuleType.Vehicle, CavernBenefit.VehicleModule)
  /** Equipment module (blue) */
  case object VanuModuleWeapon extends CarriableModule(value = "VanuModuleWeapon", VanuModuleType.Weapon, CavernBenefit.EquipmentModule)
  /** Health module (yellow) */
  case object VanuModuleEnergy extends CarriableModule(value = "VanuModuleEnergy", VanuModuleType.Healing, CavernBenefit.HealthModule)
  /** Pain module (beige) */
  case object VanuModulePain extends CarriableModule(value = "VanuModulePain", VanuModuleType.Pain, CavernBenefit.PainModule)
  /** Not really a module, don't know what this is */
  case object VanuModuleBind extends CarriableFlag(value = "VanuModuleBind", FlagCategory.VanuModuleElement)
  /** Not really a module, don't know what this is */
  case object VanuModuleFortifier extends CarriableFlag(value = "VanuModuleFortifier", FlagCategory.VanuModuleElement)

  /** Mysterious MacGuffins tied to the Bending */
  case object MonolithUnit extends CarriableFlag(value = FlagCategory.MonolithUnit.value, FlagCategory.MonolithUnit)

  /** Special event scoring tool pyon~ */
  case object RabbitBall extends CarriableFlag(value = FlagCategory.RabbitBall.value, FlagCategory.RabbitBall)

  private lazy val vanuModules: List[CarriableModule] = List(
    VanuModuleSpeed, VanuModuleDefender, VanuModuleVehicle, VanuModuleWeapon, VanuModuleEnergy, VanuModulePain
  )
  lazy val values: List[CarriableFlag] = List(
    CaptureFlag, VanuModuleBind, VanuModuleFortifier, MonolithUnit, RabbitBall
  ) ++ vanuModules

  def fromFlagType(flagType: VanuModuleType): Option[CarriableFlag] = {
    vanuModules.find { _.module == flagType }
  }

  def fromCavernBenefit(benefit: CavernBenefit): Option[CarriableModule] = {
    vanuModules.find { _.benefit == benefit }
  }
}

/**
 * All flags have only one type.
 * All containers that can receive only one type.
 */
trait DesignatedFlagType {
  def ValidFlagType: CarriableFlag
}
