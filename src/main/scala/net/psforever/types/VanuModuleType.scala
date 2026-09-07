// Copyright (c) 2026 PSForever
package net.psforever.types

import enumeratum.values.{IntEnum, IntEnumEntry}
import net.psforever.packet.PacketHelpers
import scodec.Codec
import scodec.codecs.uint4

sealed abstract class VanuModuleType(val value: Int) extends IntEnumEntry

object VanuModuleType extends IntEnum[VanuModuleType] {
  /* 0, a-f are all invalid; only 0 is associated */
  case object Invalid extends VanuModuleType(value = 0)

  case object NonPowered extends VanuModuleType(value = 1)

  case object Speed extends VanuModuleType(value = 2)

  case object Defender extends VanuModuleType(value = 3)

  case object Vehicle extends VanuModuleType(value = 4)

  case object Weapon extends VanuModuleType(value = 5)

  case object Healing extends VanuModuleType(value = 6)

  case object Pain extends VanuModuleType(value = 7)

  case object BindPoint extends VanuModuleType(value = 8)

  case object Fortifier extends VanuModuleType(value = 9)

  lazy val values: IndexedSeq[VanuModuleType] = findValues

  implicit val codec: Codec[VanuModuleType] = PacketHelpers.createIntEnumCodec(this, uint4)
}
