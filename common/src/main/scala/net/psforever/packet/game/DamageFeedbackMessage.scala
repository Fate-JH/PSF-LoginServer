// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

final case class FeedbackEntry(unk1 : Boolean,
                               unk2 : Boolean,
                               unk3 : Option[PlanetSideGUID],
                               unk4 : Option[String],
                               unk5 : Option[Int])

final case class DamageFeedbackMessage(unk1 : Int,
                                       unk2 : FeedbackEntry,
                                       unk3 : FeedbackEntry,
                                       unk4 : Option[Int],
                                       unk5 : Int,
                                       unk6 : Long,
                                       unk7 : Int)
  extends PlanetSideGamePacket {
  type Packet = DamageFeedbackMessage
  def opcode = GamePacketOpcode.DamageFeedbackMessage
  def encode = DamageFeedbackMessage.encode(this)
}

object FeedbackEntry {
  def apply(guid : PlanetSideGUID) : FeedbackEntry = {
    FeedbackEntry(true, true, Some(guid), None, None) //according to packet, unk2 is true when unk1 is true
  }

  def apply(str : String) : FeedbackEntry = {
    FeedbackEntry(false, true, None, Some(str), None)
  }

  def apply(number : Int) : FeedbackEntry = {
    FeedbackEntry(false, false, None, None, Some(number))
  }

  val codec : Codec[FeedbackEntry] = (
    ("unk1" | bool) >>:~ { unk1 =>
      ("unk2" | bool) >>:~ { unk2 =>
        conditional(unk1, "unk3" | PlanetSideGUID.codec) ::
          conditional(!unk1 && unk2, "unk4" | PacketHelpers.encodedWideString) ::
          conditional(!(unk1 && unk2), "unk5" | uintL(11))
      }
    }
    ).exmap[FeedbackEntry] (
    {
      case true :: _ :: None :: _ :: _ :: HNil =>
        Attempt.Failure(Err("entry must have globally unique identifier as data"))

      case false :: true :: _ :: None :: _ :: HNil =>
        Attempt.Failure(Err("entry must have string as data"))

      case false :: false :: _ :: _ :: None :: HNil =>
        Attempt.Failure(Err("entry must have number as data"))

      case u1 :: u2 :: u3 :: u4 :: u5 :: HNil =>
        Attempt.Successful(FeedbackEntry(u1, u2, u3, u4, u5))
    },
    {
      case FeedbackEntry(true, _, None, _, _) =>
        Attempt.Failure(Err("entry must have globally unique identifier as data"))

      case FeedbackEntry(false, true, _, None, _) =>
        Attempt.Failure(Err("entry must have string as data"))

      case FeedbackEntry(false, false, _, _, None) =>
        Attempt.Failure(Err("entry must have number as data"))

      case FeedbackEntry(u1, u2, u3, u4, u5) =>
        Attempt.Successful(u1 :: u2 :: u3 :: u4 :: u5 :: HNil)
    }
  )
}

object DamageFeedbackMessage extends Marshallable[DamageFeedbackMessage] {
  implicit val codec : Codec[DamageFeedbackMessage] = (
    ("unk1" | uint4L) ::
      ("unk2" | FeedbackEntry.codec) ::
      (("unk3" | FeedbackEntry.codec) >>:~ { unk3 =>
        conditional(!unk3.unk1, "unk4" | uint2L) ::
          ("unk5" | uintL(3)) ::
          ("unk6" | uint32L) ::
          ("unk7" | uint2L)
      })
    ).exmap[DamageFeedbackMessage] (
    {
      case _ :: _ :: FeedbackEntry(false, _, _, _, _) :: None :: _ :: _ :: _ :: HNil =>
        Attempt.Failure(Err("second entry suggests a further data field"))

      case u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: HNil =>
        Attempt.Successful(DamageFeedbackMessage(u1, u2, u3, u4, u5, u6, u7))
    },
    {
      case DamageFeedbackMessage(_, _, FeedbackEntry(false, _, _, _, _), None, _, _, _) =>
        Attempt.Failure(Err("second entry suggests a further data field"))

      case DamageFeedbackMessage(u1, u2, u3, u4, u5, u6, u7) =>
        Attempt.Successful(u1 :: u2 :: u3 :: u4 :: u5 :: u6 :: u7 :: HNil)
    }
  )
}
