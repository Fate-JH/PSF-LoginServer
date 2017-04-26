// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import net.psforever.types.PlanetSideEmpire
import scodec.{Attempt, Codec, Err}
import scodec.codecs._

/**
  * An `Enumeration` of benefits that arise because of faction continental domination.
  */
object BenefitCondition extends Enumeration {
  type Type = Value

  val LockedCyssor,
      LockedIshundar,
      LockedSearhus,
      LockedBattleIslands,
      //very specific language; the faction -> benefit association must still be explicit
      TrLockedNcHomeContinents,
      TrLockedVsHomeContinents,
      NcLockedTrHomeContinents,
      NcLockedVsHomeContinents,
      VsLockedTrHomeContinents,
      VsLockedNcHomeContinents
      = Value

  //custom codec of type: Codec[String] -> EmpireBenefit.Value
  implicit val codec = PacketHelpers.encodedStringAligned(6).exmap[BenefitCondition.Value] (
    {
      case "lock-i1-i2-i3-i4" => Attempt.successful(LockedBattleIslands)
      case "lock-z3" => Attempt.successful(LockedCyssor)
      case "lock-z4" => Attempt.successful(LockedIshundar)
      case "lock-z9" => Attempt.successful(LockedSearhus)
      case "tr-nc-vehicles" => Attempt.successful(TrLockedNcHomeContinents)
      case "tr-vs-vehicles" => Attempt.successful(TrLockedVsHomeContinents)
      case "nc-tr-vehicles" => Attempt.successful(NcLockedTrHomeContinents)
      case "nc-vs-vehicles" => Attempt.successful(NcLockedVsHomeContinents)
      case "vs-tr-vehicles" => Attempt.successful(VsLockedTrHomeContinents)
      case "vs-nc-vehicles" => Attempt.successful(VsLockedNcHomeContinents)
      case str => Attempt.failure(Err(s"$str is not part of this Enumeration"))
    },
    {
      case LockedBattleIslands => Attempt.successful("lock-i1-i2-i3-i4")
      case LockedCyssor => Attempt.successful("lock-z3")
      case LockedIshundar => Attempt.successful("lock-z4")
      case LockedSearhus => Attempt.successful("lock-z9")
      case TrLockedNcHomeContinents => Attempt.successful("tr-nc-vehicles")
      case TrLockedVsHomeContinents => Attempt.successful("tr-vs-vehicles")
      case NcLockedTrHomeContinents => Attempt.successful("nc-tr-vehicles")
      case NcLockedVsHomeContinents => Attempt.successful("nc-vs-vehicles")
      case VsLockedTrHomeContinents => Attempt.successful("vs-tr-vehicles")
      case VsLockedNcHomeContinents => Attempt.successful("vs-nc-vehicles")
      case enum => Attempt.failure(Err(s"$enum is not defined"))
    }
  )
}

/**
  * An entry that awards a continental domination benefit to a specific faction.
  * @param empire the faction
  * @param condition the reason for this benefit;
  *                  which continent has been locked/dominated
  */
final case class ContinentBenefit(empire : PlanetSideEmpire.Value,
                                    condition : BenefitCondition.Value)

/**
  * na
  * @param empire the faction
  * @param unk na
  */
final case class EmpireBenefitsExtra(empire : PlanetSideEmpire.Value,
                                     unk : Int)

/**
  * na
  * @param cont_perks a `List` of special continents captured by a faction
  * @param unk na
  */
final case class EmpireBenefitsMessage(cont_perks : List[ContinentBenefit],
                                       unk : List[EmpireBenefitsExtra])
  extends PlanetSideGamePacket {
  type Packet = EmpireBenefitsMessage
  def opcode = GamePacketOpcode.EmpireBenefitsMessage
  def encode = EmpireBenefitsMessage.encode(this)
}

object EmpireBenefitsMessage extends Marshallable[EmpireBenefitsMessage] {
  private val extra1_codec : Codec[ContinentBenefit] = (
    ("empire" | PlanetSideEmpire.codec) ::
      ("benefit" | BenefitCondition.codec)
  ).as[ContinentBenefit]

  private val extra2_codec : Codec[EmpireBenefitsExtra] = (
    ("empire" | PlanetSideEmpire.codec) ::
      ("unk" | uint16L)
    ).as[EmpireBenefitsExtra]

  implicit val codec : Codec[EmpireBenefitsMessage] = (
    ("cont_perks" | PacketHelpers.listOfNAligned(uint32L, 0, extra1_codec)) ::
      ("unk" | PacketHelpers.listOfNAligned(uint32L, 0, extra2_codec))
    ).as[EmpireBenefitsMessage]
}
