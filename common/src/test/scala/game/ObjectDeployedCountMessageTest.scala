// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import scodec.bits._

class ObjectDeployedCountMessageTest extends Specification {
  val string = hex"87 2e00 02000000 3f020000 00000000 3a030000 00000000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case ObjectDeployedCountMessage(guid : PlanetSideGUID, list : List[DeploymentCount]) =>
        guid mustEqual PlanetSideGUID(46)
        list.size mustEqual 2
        //0
        list.head.objClass mustEqual 575
        list.head.unk mustEqual 0L
        //1
        list(1).objClass mustEqual 826
        list(1).unk mustEqual 0L
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = ObjectDeployedCountMessage(PlanetSideGUID(46), DeploymentCount(575, 0L) :: DeploymentCount(826, 0L) :: Nil)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
