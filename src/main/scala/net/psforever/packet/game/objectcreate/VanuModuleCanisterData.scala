// Copyright (c) 2026 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.Marshallable
import net.psforever.types.VanuModuleType
import scodec.codecs._
import scodec.{Codec, TransformSyntax}

final case class VanuModuleCanisterData(
                                         data: CommonFieldDataWithPlacement,
                                         unk: Long,
                                         module_type: VanuModuleType
                                       ) extends ConstructorData {
  override def bitsize: Long = {
    36L + data.bitsize
  }
}

object VanuModuleCanisterData extends Marshallable[VanuModuleCanisterData] {
  implicit val codec: Codec[VanuModuleCanisterData] = (
    ("data" | CommonFieldDataWithPlacement.codec) ::
      ("unk" | uint32L) ::
      ("module_type" | VanuModuleType.codec)
    ).as[VanuModuleCanisterData]
}
