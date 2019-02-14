// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.bits.BitVector
import scodec.{Attempt, Codec}
import scodec.codecs._
import shapeless.{::, HNil}

//TODO Write more some other time
/**
  * Dispatched by the server to enact an effect on some game object.<br>
  * <br>
  * __Action Codes__:<br>
  *  From server:<br>
  *  - 96, 97, 98, 99 - the vehicle bounces slightly; have seen this in packet captures after taking a vehicle through a warpgate<br>
  *  - 200, 201, 202, 203 - For aircraft - client shows "The bailing mechanism failed! To fix the mechanism, land and repair the vehicle!"<br>
  *  - 224 - sets vehicle or player to be black ops<br>
  *  - 228 - reverts player from black ops<br>
  *  From client:<br>
  *  - 156 - for BFR's, disable an enabled arm weapon
  *  - 152 - for BFR's, enable a disabled arm weapon
  *  - 176 - for BFR's, animates the energy shield
  * @param object_guid the target object
  * @param code the action code
  */
final case class GenericObjectActionMessage(object_guid : PlanetSideGUID,
                                            code : Int)
  extends PlanetSideGamePacket {
  type Packet = GenericObjectActionMessage
  def opcode = GamePacketOpcode.GenericObjectActionMessage
  def encode = GenericObjectActionMessage.encode(this)
}

object GenericObjectActionMessage extends Marshallable[GenericObjectActionMessage] {
  implicit val codec : Codec[GenericObjectActionMessage] = (
    ("object_guid" | PlanetSideGUID.codec) ::
      ("code" | uint8L) ::
      ("ex" | bits) //"code" may extract at odd sizes
    ).exmap[GenericObjectActionMessage] (
    {
      case guid :: code :: _ :: HNil =>
        Attempt.Successful(GenericObjectActionMessage(guid, code))
    },
    {
      case GenericObjectActionMessage(guid, code) =>
        Attempt.Successful(guid :: code :: BitVector.empty :: HNil)
    }
  )
}
