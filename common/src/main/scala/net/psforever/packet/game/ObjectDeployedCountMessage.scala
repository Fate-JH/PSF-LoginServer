// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._
//import shapeless.{::, HNil}

/**
  * A deployable object's `List` entry.
  * @param objClass the type of deployable item
  * @param unk na
  */
final case class DeploymentCount(objClass : Long,
                                 unk : Long)

/**
  * na
  * @param guid na
  * @param deployments a `List` of `DeploymentCount` data
  */
final case class ObjectDeployedCountMessage(guid : PlanetSideGUID,
                                            deployments : List[DeploymentCount]
                                           ) extends PlanetSideGamePacket {
  type Packet = ObjectDeployedCountMessage
  def opcode = GamePacketOpcode.ObjectDeployedCountMessage
  def encode = ObjectDeployedCountMessage.encode(this)
}

object ObjectDeployedCountMessage extends Marshallable[ObjectDeployedCountMessage] {
  //TODO use this when ObjectClass has been converted from an object containing numeric constants to an Enumeration?
//  private val objClass_32u : Codec[Long] = uintL(11).hlist.xmap[Long] (
//    {
//      case n :: HNil =>
//        n.toLong
//    },
//    {
//      case n =>
//        n.toInt :: HNil
//    }
//  )

  /**
    * `Codec` for `DeploymentCount` data.
    */
  final val count_codec : Codec[DeploymentCount] = (
    ("objClass" | uint32L) ::
      ("unk" | uint32L)
    ).as[DeploymentCount]

  implicit val codec : Codec[ObjectDeployedCountMessage] = (
    ("guid" | PlanetSideGUID.codec) ::
      ("deployments" | PacketHelpers.listOfNAligned(uint32L, 0, count_codec))
    ).as[ObjectDeployedCountMessage]
}
