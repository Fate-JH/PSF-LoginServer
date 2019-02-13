// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.types.Vector3
import scodec.bits._

class FrameVehicleStateMessageTest extends Specification {
  val string = hex"1c bf01 0416593b887f20600038de5e606483f5f00a0000000000000000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case FrameVehicleStateMessage(guid, unk1, pos, orient, vel, unk2, unk3, unk4, unk5, unk6, unk7, unk8, unk9, unkA) =>
        guid mustEqual PlanetSideGUID(447)
        unk1 mustEqual 0
        pos mustEqual Vector3(6500.25f, 1929.2266f, 15.890625f)
        orient mustEqual Vector3(0 ,357.53906f, 50.976562f)
        vel.contains(Vector3(3.78125f ,3.125f ,-0.09375f)) mustEqual true
        unk2 mustEqual false
        unk3 mustEqual 0
        unk4 mustEqual 0
        unk5 mustEqual false
        unk6 mustEqual false
        unk7 mustEqual false
        unk8 mustEqual 10
        unk9 mustEqual 0L
        unkA mustEqual 0L
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = FrameVehicleStateMessage(
      PlanetSideGUID(447),
      0,
      Vector3(6500.25f, 1929.2266f, 15.890625f),
      Vector3(0 ,357.53906f, 50.976562f),
      Some(Vector3(3.78125f ,3.125f ,-0.09375f)),
      false,
      0, 0,
      false, false, false,
      10,
      0L, 0L
    )
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
