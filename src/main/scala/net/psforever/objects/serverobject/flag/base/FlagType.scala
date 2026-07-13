// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import enumeratum.values.{StringEnum, StringEnumEntry}

sealed abstract class FlagType(override val value: String, category: FlagCategory) extends StringEnumEntry

object FlagType extends StringEnum[FlagType] {
  val values: IndexedSeq[FlagType] = findValues

  case object CaptureFlag extends FlagType(value = "CaptureFlag", FlagCategory.CaptureFlag)

  /** Not really a module, rather the cavity in which a module is stored */
  case object VanuModuleCradle extends FlagType(value = "VanuModuleCradle", FlagCategory.VanuModule)
  /** Pain module (beige) */
  case object VanuModuleBind extends FlagType(value = "VanuModuleBind", FlagCategory.VanuModule)
  /** Shield module (aqua) */
  case object VanuModuleDefender extends FlagType(value = "VanuModuleDefender", FlagCategory.VanuModule)
  /** Speed module (orange) */
  case object VanuModuleEnergy extends FlagType(value = "VanuModuleEnergy", FlagCategory.VanuModule)
  /** Health module (yellow) */
  case object VanuModuleFortifier extends FlagType(value = "VanuModuleFortifier", FlagCategory.VanuModule)
  /** Vehicle module (purple) */
  case object VanuModuleVehicle extends FlagType(value = "VanuModuleVehicle", FlagCategory.VanuModule)
  /** Equipment module (blue) */
  case object VanuModuleWeapon extends FlagType(value = "VanuModuleWeapon", FlagCategory.VanuModule)

  case object MonolithUnit extends FlagType(value = "MonolithUnit", FlagCategory.MonolithUnit)

  case object RabbitBall extends FlagType(value = "RabbitBall", FlagCategory.RabbitBall)
}

sealed abstract class FlagCategory(override val value: String) extends StringEnumEntry

object FlagCategory extends StringEnum[FlagCategory] {
  val values: IndexedSeq[FlagCategory] = findValues

  /** The lattice logic unit (LLU).*/
  case object CaptureFlag extends FlagCategory(value = "CaptureFlag")
  /** Special enhancement modules generated in cavern facilities to be installed into above ground facilities. */
  case object VanuModule extends FlagCategory(value = "VanuModule")
  /** Mysterious MacGuffins tied to the Bending. */
  case object MonolithUnit extends FlagCategory(value = "MonolithUnit")
  /** Pyon~~ */
  case object RabbitBall extends FlagCategory(value = "RabbitBall")
}

/**
 * All flags have only one type.
 * All containers that can receive only one type.
 */
trait DesignatedFlagType {
  def ValidFlagType: FlagType
}
