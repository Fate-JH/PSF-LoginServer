// Copyright (c) 2021 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import scodec.bits._

class OutfitRequestTest extends Specification {
  val string0 = hex"8e 02b54f40401780560061006e00750020006f0075007400660069007400200066006f0072002000740068006500200070006c0061006e00650074007300690064006500200066006f00720065007600650072002000700072006f006a006500630074002100200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002d00660069006e00640020006f007500740020006d006f00720065002000610062006f0075007400200074006800650020005000530045004d0055002000700072006f006a0065006300740020006100740020005000530066006f00720065007600650072002e006e0065007400"
  val string1 = hex"8e 310030002e00200052006f00730065002d004d002c0020004e0043001237c000a4310031002e00200054006800650052006500640046006c0061006d0069006e0067006f002c0020005400520020003c00200046006f00720073006500720061006c0020003e0020001237c00093310032002e0020004c00410050007800690061006f006c00690061006e002c0020004e0043001237c000a3310033002e002000530063007200610077006e00790052006f006e006e00690065002c0020005400520020003c00200046006f00720073006500720061006c0020003e0020001237c000a2310034002e00200041004f0044005300630069006f006e0032003000310030002c0020005400520020003c00200046006f00720073006500720061006c0020003e0020001237c00093310035002e0020004b0069006e0067004e0065007000740075006e0065002c002000560053001237c0009e310036002e00200044004e005300520041005400580035002c0020005400520020003c00200046006f00720073006500720061006c0020003e0020001237c0009c310037002e00200049006e006f007800690063002c0020005400520020003c00200046006f00720073006500720061006c0020003e"
  val string3 = hex"8e 649e822010"
  val string4 = hex"8e 90412f4050"

  "decode (0)" in {
    PacketCoding.decodePacket(string0).require match {
      case OutfitRequest(unk, info) =>
        unk mustEqual 41593365
        info mustEqual OutfitInfo0("Vanu outfit for the planetside forever project!                                                                                      -find out more about the PSEMU project at PSforever.net")
      case _ =>
        ko
    }
  }

//  "decode (1)" in {
//    PacketCoding.decodePacket(string1).require match {
//      case OutfitRequest(unk, info) =>
//        unk mustEqual 25166216
//        info mustEqual OutfitInfo1(List(None))
//      case _ =>
//        ko
//    }
//  }

  "decode (3)" in {
    PacketCoding.decodePacket(string3).require match {
      case OutfitRequest(unk, info) =>
        unk mustEqual 1176612
        info mustEqual OutfitInfo3(true)
      case _ =>
        ko
    }
  }

  "decode (4)" in {
    PacketCoding.decodePacket(string4).require match {
      case OutfitRequest(unk, info) =>
        unk mustEqual 41552258
        info mustEqual InfoWindowVisibility(true)
      case _ =>
        ko
    }
  }

  "encode (0)" in {
    val msg = OutfitRequest(41593365, OutfitInfo0(
      "Vanu outfit for the planetside forever project!                                                                                      -find out more about the PSEMU project at PSforever.net"
    ))
    val pkt = PacketCoding.encodePacket(msg).require.toByteVector

    pkt mustEqual string0
  }

  "encode (3)" in {
    val msg = OutfitRequest(1176612, OutfitInfo3(true))
    val pkt = PacketCoding.encodePacket(msg).require.toByteVector

    pkt mustEqual string3
  }

  "encode (4)" in {
    val msg = OutfitRequest(41552258, InfoWindowVisibility(true))
    val pkt = PacketCoding.encodePacket(msg).require.toByteVector

    pkt mustEqual string4
  }
}

