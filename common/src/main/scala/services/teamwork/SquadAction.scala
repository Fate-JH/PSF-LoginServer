// Copyright (c) 2019 PSForever
package services.teamwork

import net.psforever.objects.Player
import net.psforever.packet.game._
import net.psforever.types.{SquadRequestType, Vector3}

object SquadAction {
  trait Action

  final case class Definition(player : Player, zone_ordinal_number : Int, guid : PlanetSideGUID, line : Int, action : SquadAction) extends Action
  final case class Membership(request_type : SquadRequestType.Value, unk2 : Long, unk3 : Option[Long], player_name : String, unk5 : Option[Option[String]]) extends Action
  final case class Update(char_id : Long, health : Int, max_health : Int, armor : Int, max_armor : Int, pos : Vector3, zone_number : Int) extends Action
}
