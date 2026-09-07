// Copyright (c) 2026 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.Marshallable
import net.psforever.types.VanuModuleType
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Codec, TransformSyntax}

final case class VanuModuleCanisterData(
                                         data: CommonFieldDataWithPlacement,
                                         u1: Long,
                                         module_type: VanuModuleType,
                                         u3: ByteVector
                                       ) extends ConstructorData {
  override def bitsize: Long = {
    36L + data.bitsize + u3.size
  }
}

object VanuModuleCanisterData extends Marshallable[VanuModuleCanisterData] {
  implicit val codec: Codec[VanuModuleCanisterData] = (
    ("data" | CommonFieldDataWithPlacement.codec) ::
      ("u1" | uint32L) ::
      ("module_type" | VanuModuleType.codec) ::
      bytes
    ).as[VanuModuleCanisterData]
}
