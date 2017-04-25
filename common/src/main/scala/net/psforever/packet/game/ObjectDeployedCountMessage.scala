// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/*
8783a81b0d141700b82435b6e30002c9befb1f0483cdf9c00380081c0fa8fcbdd22d8e132cff007ed008451bc2fbb4020d1b132a04817708
 */

final case class DeploymentCount(unk1 : Long,
                                 unk2 : Long)

final case class ObjectDeployedCountMessage(player_guid : PlanetSideGUID,
                                            deployments : List[DeploymentCount]
                                           ) extends PlanetSideGamePacket {
  type Packet = ObjectDeployedCountMessage
  def opcode = GamePacketOpcode.ObjectDeployedCountMessage
  def encode = ObjectDeployedCountMessage.encode(this)
}

object ObjectDeployedCountMessage extends Marshallable[ObjectDeployedCountMessage] {
  final val count_codec : Codec[DeploymentCount] = (
    ("unk1" | uint32L) ::
      ("unk2" | uint32L)
    ).as[DeploymentCount]

  implicit val codec : Codec[ObjectDeployedCountMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("deployments" | PacketHelpers.listOfNAligned(uint32L, 0, count_codec))
    ).as[ObjectDeployedCountMessage]
}
