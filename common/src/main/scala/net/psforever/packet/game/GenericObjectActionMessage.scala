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
  *  - 90? - for observed driven BFR's, model pitches up slightly and stops idle animation<br>
  *  - 96, 97, 98, 99 - the vehicle bounces slightly; have seen this in packet captures after taking a vehicle through a warpgate<br>
  *  - 100 - for observed driven BFR's, model resets animation following GOAM90?<br>
  *  - 136, 137, 138, 139 - "Time item can be used" messages (with default times)<br>
  *  - 176, 179 - for BFR's, animates the energy shield<br>
  *  - 180 - for BFR's, energy shield dissipates<br>
  *  - 190?, 191?, 192?, 193?, 194? - Control Interface unstable messages<br>
  *  - 195, 196, 197 - Control Interface malfunction messages<br>
  *  - 184 - for BFR's, four explosions 5s apart on the machine's abdomen<br>
  *  - 200, 201, 202, 203 - for aircraft, "The bailing mechanism failed! To fix the mechanism, land and repair the vehicle!" message<br>
  *  - 220 - ??? (client responds with GOAM29)<br>
  *  - 224 - sets vehicle or player to be black ops<br>
  *  - 228 - reverts player from black ops<br>
  *  From client:<br>
  *  - 29 - unknown (response to GOAM220)<br>
  *  Both ways:<br>
  *  - 156 - for BFR's, disable an enabled arm weapon<br>
  *  - 152 - for BFR's, enable a disabled arm weapon
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
