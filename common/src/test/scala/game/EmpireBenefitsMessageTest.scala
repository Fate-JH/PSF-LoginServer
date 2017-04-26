// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game.{EmpireBenefitsMessage, ContinentBenefit, EmpireBenefitsExtra, BenefitCondition}
import net.psforever.types.PlanetSideEmpire
import scodec.bits._

class EmpireBenefitsMessageTest extends Specification {
  val string = hex"d7 04000000 24006c6f636b2d69312d69322d69332d693421c06c6f636b2d7a3361c06c6f636b2d7a3961c06c6f636b2d7a34 04000000 010000600404010300"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case EmpireBenefitsMessage(cont_perks, unk) =>
        cont_perks.size mustEqual 4
        //0
        cont_perks.head.empire mustEqual PlanetSideEmpire.TR
        cont_perks.head.condition mustEqual BenefitCondition.LockedBattleIslands
        //1
        cont_perks(1).empire mustEqual PlanetSideEmpire.TR
        cont_perks(1).condition mustEqual BenefitCondition.LockedCyssor
        //2
        cont_perks(2).empire mustEqual PlanetSideEmpire.NC
        cont_perks(2).condition mustEqual BenefitCondition.LockedSearhus
        //3
        cont_perks(3).empire mustEqual PlanetSideEmpire.NC
        cont_perks(3).condition mustEqual BenefitCondition.LockedIshundar

        unk.size mustEqual 4
        //0
        unk.head.empire mustEqual PlanetSideEmpire.TR
        unk.head.unk mustEqual 4
        //1
        unk(1).empire mustEqual PlanetSideEmpire.TR
        unk(1).unk mustEqual 6
        //2
        unk(2).empire mustEqual PlanetSideEmpire.NC
        unk(2).unk mustEqual 1
        //3
        unk(3).empire mustEqual PlanetSideEmpire.NC
        unk(3).unk mustEqual 3
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = EmpireBenefitsMessage(
      ContinentBenefit(PlanetSideEmpire.TR, BenefitCondition.LockedBattleIslands) ::
        ContinentBenefit(PlanetSideEmpire.TR, BenefitCondition.LockedCyssor) ::
        ContinentBenefit(PlanetSideEmpire.NC, BenefitCondition.LockedSearhus) ::
        ContinentBenefit(PlanetSideEmpire.NC, BenefitCondition.LockedIshundar) ::
        Nil,
      EmpireBenefitsExtra(PlanetSideEmpire.TR, 4) ::
        EmpireBenefitsExtra(PlanetSideEmpire.TR, 6) ::
        EmpireBenefitsExtra(PlanetSideEmpire.NC, 1) ::
        EmpireBenefitsExtra(PlanetSideEmpire.NC, 3) ::
        Nil
    )
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
