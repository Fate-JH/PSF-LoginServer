// Copyright (c) 2017 PSForever
package objects

import akka.actor.{Actor, Props}
import base.ActorTest
import net.psforever.objects._
import net.psforever.objects.avatar.Avatar
import net.psforever.objects.serverobject.terminals.Terminal
import net.psforever.objects.serverobject.tube.SpawnTube
import net.psforever.objects.vehicles._
import net.psforever.packet.game.ItemTransactionMessage
import net.psforever.types._
import org.specs2.mutable._

import scala.concurrent.duration.Duration

class UtilityTest extends Specification {
  "Utility" should {
    "create an order_terminala object" in {
      val obj = Utility(UtilityType.order_terminala, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.order_terminala
      obj().isInstanceOf[Terminal] mustEqual true
      obj().asInstanceOf[Terminal].Definition.ObjectId mustEqual 613
      obj().asInstanceOf[Terminal].Actor mustEqual Default.Actor
    }

    "create an order_terminalb object" in {
      val obj = Utility(UtilityType.order_terminalb, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.order_terminalb
      obj().isInstanceOf[Terminal] mustEqual true
      obj().asInstanceOf[Terminal].Definition.ObjectId mustEqual 614
      obj().asInstanceOf[Terminal].Actor mustEqual Default.Actor
    }

    "create a matrix_terminalc object" in {
      val obj = Utility(UtilityType.matrix_terminalc, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.matrix_terminalc
      obj().isInstanceOf[Terminal] mustEqual true
      obj().asInstanceOf[Terminal].Definition.ObjectId mustEqual 519
      obj().asInstanceOf[Terminal].Actor mustEqual Default.Actor
    }

    "create an ams_respawn_tube object" in {
      val obj = Utility(UtilityType.ams_respawn_tube, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.ams_respawn_tube
      obj().isInstanceOf[SpawnTube] mustEqual true
      obj().asInstanceOf[SpawnTube].Definition.ObjectId mustEqual 49
      obj().asInstanceOf[SpawnTube].Actor mustEqual Default.Actor
    }

    "create a teleportpad_terminal object" in {
      val obj = Utility(UtilityType.teleportpad_terminal, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.teleportpad_terminal
      obj().isInstanceOf[Terminal] mustEqual true
      obj().asInstanceOf[Terminal].Definition.ObjectId mustEqual 853
      obj().asInstanceOf[Terminal].Actor mustEqual Default.Actor
    }

    "produce a telepad object through the teleportpad_terminal" in {
      val veh = Vehicle(GlobalDefinitions.quadstealth)
      veh.Faction = PlanetSideEmpire.TR
      val obj    = Utility(UtilityType.teleportpad_terminal, UtilityTest.vehicle)
      val player = Player(Avatar(0, "TestCharacter", PlanetSideEmpire.TR, CharacterSex.Male, 0, CharacterVoice.Mute))
      veh.GUID = PlanetSideGUID(101)
      obj().Owner = veh //hack
      obj().GUID = PlanetSideGUID(1)
      player.GUID = PlanetSideGUID(2)

      val msg = obj()
        .asInstanceOf[Terminal]
        .Request(
          player,
          ItemTransactionMessage(PlanetSideGUID(853), TransactionType.Buy, 0, "router_telepad", 0, PlanetSideGUID(0))
        )
      msg.isInstanceOf[Terminal.BuyEquipment] mustEqual true
      msg.asInstanceOf[Terminal.BuyEquipment].item.isInstanceOf[Telepad] mustEqual true
    }

    "create an internal_router_telepad_deployable object" in {
      val obj = Utility(UtilityType.internal_router_telepad_deployable, UtilityTest.vehicle)
      obj.UtilType mustEqual UtilityType.internal_router_telepad_deployable
      obj().isInstanceOf[Utility.InternalTelepad] mustEqual true
      obj().asInstanceOf[Utility.InternalTelepad].Definition.ObjectId mustEqual 744
      obj().asInstanceOf[Utility.InternalTelepad].Actor mustEqual Default.Actor
    }

    "internal_router_telepad_deployable can keep track of an object's GUID (presumedly, it's a Telepad)" in {
      val obj   = Utility(UtilityType.internal_router_telepad_deployable, UtilityTest.vehicle)
      val inpad = obj().asInstanceOf[Utility.InternalTelepad]

      inpad.Telepad.isEmpty mustEqual true
      inpad.Telepad = PlanetSideGUID(5)
      inpad.Telepad.contains(PlanetSideGUID(5)) mustEqual true
      inpad.Telepad = PlanetSideGUID(6)
      inpad.Telepad.contains(PlanetSideGUID(6)) mustEqual true
      inpad.Telepad = None
      inpad.Telepad.isEmpty mustEqual true
    }

    "be located with their owner (terminal)" in {
      val veh = Vehicle(GlobalDefinitions.quadstealth)
      val obj = Utility(UtilityType.order_terminala, veh)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation

      veh.Position = Vector3(1, 2, 3)
      veh.Orientation = Vector3(4, 5, 6)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation
    }

    "be located with their owner (spawn tube)" in {
      val veh = Vehicle(GlobalDefinitions.quadstealth)
      val obj = Utility(UtilityType.ams_respawn_tube, veh)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation

      veh.Position = Vector3(1, 2, 3)
      veh.Orientation = Vector3(4, 5, 6)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation
    }

    "be located with their owner (internal telepad)" in {
      val veh = Vehicle(GlobalDefinitions.quadstealth)
      val obj = Utility(UtilityType.internal_router_telepad_deployable, veh)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation

      veh.Position = Vector3(1, 2, 3)
      veh.Orientation = Vector3(4, 5, 6)
      veh.GUID = PlanetSideGUID(101)
      obj().Position mustEqual veh.Position
      obj().Orientation mustEqual veh.Orientation
      obj().asInstanceOf[Utility.InternalTelepad].Router.contains(veh.GUID) mustEqual true
    }
  }
}

class UtilityTerminalATest extends ActorTest {
  "Utility" should {
    "wire an order_terminala Actor" in {
      val obj = Utility(UtilityType.order_terminala, UtilityTest.vehicle)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
    }
  }
}

class UtilityTerminalBTest extends ActorTest {
  "Utility" should {
    "wire an order_terminalb Actor" in {
      val obj = Utility(UtilityType.order_terminalb, UtilityTest.vehicle)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
    }
  }
}

class UtilityTerminalCTest extends ActorTest {
  "Utility" should {
    "wire a matrix_terminalc Actor" in {
      val obj = Utility(UtilityType.matrix_terminalc, UtilityTest.vehicle)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
    }
  }
}

class UtilityRespawnTubeTest extends ActorTest {
  "Utility" should {
    "wire an ams_respawn_tube Actor" in {
      val obj = Utility(UtilityType.ams_respawn_tube, UtilityTest.vehicle)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
    }
  }
}

class UtilityTelepadTerminalTest extends ActorTest {
  "Utility" should {
    "wire a teleportpad_terminal Actor" in {
      val obj = Utility(UtilityType.teleportpad_terminal, UtilityTest.vehicle)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
    }
  }
}

class UtilityInternalTelepadTest extends ActorTest {
  "Utility" should {
    "wire a teleportpad_terminal Actor" in {
      val veh = Vehicle(GlobalDefinitions.quadstealth)
      veh.GUID = PlanetSideGUID(101)
      val obj = Utility(UtilityType.internal_router_telepad_deployable, veh)
      obj().GUID = PlanetSideGUID(1)
      assert(obj().Actor == Default.Actor)
      assert(obj().asInstanceOf[Utility.InternalTelepad].Router.contains(veh.GUID))

      system.actorOf(Props(classOf[UtilityTest.SetupControl], obj), "test") ! ""
      receiveOne(Duration.create(500, "ms")) //consume and discard
      assert(obj().Actor != Default.Actor)
      assert(obj().asInstanceOf[Utility.InternalTelepad].Router.contains(veh.GUID))
    }
  }
}

object UtilityTest {
  val vehicle = Vehicle(GlobalDefinitions.quadstealth)

  class SetupControl(obj: Utility) extends Actor {
    def receive: Receive = {
      case _ =>
        obj.Setup(context)
        sender() ! ""
    }
  }
}
