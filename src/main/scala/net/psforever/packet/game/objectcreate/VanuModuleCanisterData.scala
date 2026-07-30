// Copyright (c) 2026 PSForever
package net.psforever.packet.game.objectcreate

import enumeratum.values.{IntEnum, IntEnumEntry}
import net.psforever.objects.serverobject.flag.base.FlagType
import net.psforever.packet.{Marshallable, PacketHelpers}
import net.psforever.types.CavernBenefit
import scodec.codecs._
import scodec.{Codec, TransformSyntax}

sealed abstract class FlagTypeData(
                                    val value: Int,
                                    val flag: Option[FlagType],
                                    val benefit: Option[CavernBenefit]
                                  ) extends IntEnumEntry

object FlagTypeData extends IntEnum[FlagTypeData] {
  case object Empty extends FlagTypeData(value = 1, None, None)

  case object Bind extends FlagTypeData(value = 2, Some(FlagType.VanuModuleBind), None)

  case object Defender extends FlagTypeData(value = 3, Some(FlagType.VanuModuleDefender), Some(CavernBenefit.ShieldModule))

  case object Vehicle extends FlagTypeData(value = 4, Some(FlagType.VanuModuleVehicle), Some(CavernBenefit.VehicleModule))

  case object Weapon extends FlagTypeData(value = 5, Some(FlagType.VanuModuleWeapon), Some(CavernBenefit.EquipmentModule))

  case object Healing extends FlagTypeData(value = 6, None, Some(CavernBenefit.HealthModule))

  case object Pain extends FlagTypeData(value = 7, None, Some(CavernBenefit.PainModule))

  case object BindPoint extends FlagTypeData(value = 8, None, None)

  case object Fortifier extends FlagTypeData(value = 9, Some(FlagType.VanuModuleFortifier), Some(CavernBenefit.SpeedModule))

  lazy val values: IndexedSeq[FlagTypeData] = findValues

  implicit val codec: Codec[FlagTypeData] = PacketHelpers.createIntEnumCodec(this, uint4)

  def fromFlagType(flagType: FlagType): Option[FlagTypeData] = {
    values.find { entry => entry.flag.contains(flagType) }
  }

  def fromCavernBenefit(benefit: CavernBenefit): Option[FlagTypeData] = {
    values.find { entry => entry.benefit.contains(benefit ) }
  }
}

final case class VanuModuleCanisterData(
                                         flag: CaptureFlagData,
                                         module_type: FlagTypeData,
                                         unk1: Long,
                                         unk2: Boolean,
                                         unk3: Long,
                                         unk4: Long
                                       ) extends ConstructorData {
  override def bitsize: Long = {
    flag.bitsize + 101L
  }
}

object VanuModuleCanisterData extends Marshallable[VanuModuleCanisterData] {
  def apply(
             flag: CaptureFlagData,
             module_type: FlagType,
             unk1: Long,
             unk2: Boolean,
             unk3: Long,
             unk4: Long
           ): VanuModuleCanisterData = {
    FlagTypeData
      .fromFlagType(module_type)
      .map(ftype => VanuModuleCanisterData(flag, ftype, unk1, unk2, unk3, unk4))
      .getOrElse {
        throw new IllegalArgumentException(s"VanuModuleCanisterData can not lookup flag type data for $module_type")
      }
  }

  implicit val codec: Codec[VanuModuleCanisterData] = (
    ("flag" | CaptureFlagData.codec) ::
      ("module_type" | FlagTypeData.codec) ::
      ("unk1" | uint32L) ::
      ("unk2" | bool) ::
      ("unk3" | uint32L) ::
      ("unk4" | uint32L)
    ).as[VanuModuleCanisterData]
}
