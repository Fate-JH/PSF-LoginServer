// Copyright (c) 2017 PSForever
package game

import net.psforever.packet._
import net.psforever.packet.game._
import org.specs2.mutable._
import scodec.bits._

class DamageFeedbackMessageTest extends Specification {
  val string = hex"7b 3f6c33db0c2020000000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case DamageFeedbackMessage(unk1, unk2, unk3, unk4, unk5, unk6, unk7) =>
        unk1 mustEqual 3
        unk2.unk1 mustEqual true
        unk2.unk2 mustEqual true
        unk2.unk3.isDefined mustEqual true
        unk2.unk3.get mustEqual PlanetSideGUID(3291)
        unk2.unk4.isDefined mustEqual false
        unk2.unk5.isDefined mustEqual false
        unk3.unk1 mustEqual true
        unk3.unk2 mustEqual true
        unk3.unk3.isDefined mustEqual true
        unk3.unk3.get mustEqual PlanetSideGUID(3291)
        unk3.unk4.isDefined mustEqual false
        unk3.unk5.isDefined mustEqual false
        unk4.isDefined mustEqual false
        unk5 mustEqual 1
        unk6 mustEqual 1
        unk7 mustEqual 0
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = DamageFeedbackMessage(3, FeedbackEntry(PlanetSideGUID(3291)), FeedbackEntry(PlanetSideGUID(3291)), None, 1, 1, 0)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
