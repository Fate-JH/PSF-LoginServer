// Copyright (c) 2021 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Attempt.{Failure, Successful}
import scodec.{Codec, Err}
import scodec.bits.BitVector
import scodec.codecs._
import shapeless.HNil

import scala.annotation.switch

abstract class OutfitInformation(val code: Int)

final case class BadOutfitInfo(data: BitVector) extends OutfitInformation(code = 5)

final case class OutfitInfo0(str: String) extends OutfitInformation(code = 0)

final case class OutfitInfo1(list: List[Option[String]]) extends OutfitInformation(code = 1)

final case class OutfitInfo2(unk: Int) extends OutfitInformation(code = 2)

final case class OutfitInfo3(unk: Boolean) extends OutfitInformation(code = 3)

final case class InfoWindowVisibility(visible: Boolean) extends OutfitInformation(code = 4)

final case class OutfitRequest(
                                id: Long,
                                info: OutfitInformation
                              )
  extends PlanetSideGamePacket {
  type Packet = OutfitRequest
  def opcode = GamePacketOpcode.OutfitRequest
  def encode = OutfitRequest.encode(this)
}

object OutfitRequest extends Marshallable[OutfitRequest] {
  private val badOutfitInfoCodec: Codec[BadOutfitInfo] = bits.xmap[BadOutfitInfo] (
    data => BadOutfitInfo(data),
    {
      case BadOutfitInfo(data) => data
    }
  )

  private val outfitInfo0Codec: Codec[OutfitInfo0] = PacketHelpers.encodedWideStringAligned(adjustment = 5).xmap[OutfitInfo0] (
    str => OutfitInfo0(str),
    {
      case OutfitInfo0(str) => str
    }
  )

  private val outfitInto1Codec: Codec[OutfitInfo1] = moreStrings(more = 7, offset = 3).xmap[OutfitInfo1] (
    a => OutfitInfo1(a),
    {
      case OutfitInfo1(list) => list
    }
  )

  private def moreStrings(more: Int, offset: Int): Codec[List[Option[String]]] = {
    import shapeless.::
    val newOffset = if (offset == 8) {
      1
    } else {
      offset + 1
    }
    (
      optional(bool, PacketHelpers.encodedWideStringAligned(adjustment = 8 - newOffset)) >>:~ { entry =>
        conditional(more > 0, moreStrings(more - 1, if (entry.nonEmpty) { 0 } else { newOffset } )).hlist
      }
      ).exmap[List[Option[String]]] (
      {
        case a :: Some(b) :: HNil => Successful(a +: b)
        case a :: None :: HNil => Successful(List(a))
        case _ => Failure(Err("..."))
      },
      {
        case list if list.size > 1 => Successful(list.head :: Some(list.tail) :: HNil)
        case list if list.size == 1 => Successful(list.head :: None :: HNil)
        case _ => Failure(Err("..."))
      }
    )
  }

  private val outfitInfo2Codec: Codec[OutfitInfo2] = uint8L.xmap[OutfitInfo2] (
    value => OutfitInfo2(value),
    {
      case OutfitInfo2(value) => value
    }
  )

  private val outfitInfo3Codec: Codec[OutfitInfo3] = bool.xmap[OutfitInfo3] (
    value => OutfitInfo3(value),
    {
      case OutfitInfo3(value) => value
    }
  )

  private val outfitInfo4Codec: Codec[InfoWindowVisibility] = bool.xmap[InfoWindowVisibility] (
    value => InfoWindowVisibility(value),
    {
      case InfoWindowVisibility(value) => value
    }
  )

  private def selectOutfitInformationCodec(code: Int): Codec[OutfitInformation] = {
    ((code: @switch) match {
      case 0 => outfitInfo0Codec
      case 1 => outfitInto1Codec
      case 2 => outfitInfo2Codec
      case 3 => outfitInfo3Codec
      case 4 => outfitInfo4Codec
      case _ => badOutfitInfoCodec
    }).asInstanceOf[Codec[OutfitInformation]]
  }

  implicit val codec: Codec[OutfitRequest] = {
    import shapeless.::
    (
      uint(bits = 3) >>:~ { code =>
        ("id" | uint32L) ::
        ("info" | selectOutfitInformationCodec(code))
      }
    ).xmap[OutfitRequest](
      {
        case _ :: unk :: info :: HNil => OutfitRequest(unk, info)
      },
      {
        case OutfitRequest(unk, info) => info.code :: unk :: info :: HNil
      }
    )
  }
}
