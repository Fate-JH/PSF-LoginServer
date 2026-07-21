// Copyright (c) 2026 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.objects.serverobject.flag.base.FlagType
import net.psforever.packet.Marshallable
import scodec.Attempt.{Failure, Successful}
import scodec.codecs._
import scodec.{Codec, Err, TransformSyntax}

final case class VanuModuleCanisterData(
                                         flag: CaptureFlagData,
                                         module_type: FlagType,
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
  private val module_type_codec: Codec[FlagType] = uint(bits = 4).exmap[FlagType](
    {
      case 0 => Successful(FlagType.VanuModuleBind)
      case 1 => Successful(FlagType.VanuModuleEnergy)
      case 2 => Successful(FlagType.VanuModuleVehicle)
      case 3 => Successful(FlagType.VanuModuleWeapon)
      case 4 => Successful(FlagType.VanuModuleDefender)
      case 5 => Successful(FlagType.VanuModuleFortifier)
      case n => Failure(Err(s"unknown or incorrect value for vanu module type - $n -> ?"))
    },
    {
      case FlagType.VanuModuleBind => Successful(0)
      case FlagType.VanuModuleEnergy => Successful(1)
      case FlagType.VanuModuleVehicle => Successful(2)
      case FlagType.VanuModuleWeapon => Successful(3)
      case FlagType.VanuModuleDefender => Successful(4)
      case FlagType.VanuModuleFortifier => Successful(5)
      case n => Failure(Err(s"unknown or incorrect type for vanu module - $n"))
    }
  )

  implicit val codec: Codec[VanuModuleCanisterData] = (
    ("flag" | CaptureFlagData.codec) ::
      ("module_type" | module_type_codec) ::
      ("unk1" | uint32L) ::
      ("unk2" | bool) ::
      ("unk3" | uint32L) ::
      ("unk4" | uint32L)
    ).as[VanuModuleCanisterData]
}
