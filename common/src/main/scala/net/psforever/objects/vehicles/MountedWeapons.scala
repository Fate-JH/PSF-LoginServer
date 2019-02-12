// Copyright (c) 2017 PSForever
package net.psforever.objects.vehicles

import net.psforever.objects.PlanetSideGameObject
import net.psforever.objects.equipment.{Equipment, EquipmentSlot}
import net.psforever.objects.inventory.Container
import net.psforever.objects.serverobject.mount.Mountable
import net.psforever.objects.vehicles.{Seat => Chair}

trait MountedWeapons {
  this : PlanetSideGameObject with Mountable with Container =>

  def Weapons : Map[Int, EquipmentSlot]

  /**
    * Given a valid seat number, retrieve an index where the weapon controlled from this seat is mounted.
    * @param seatNumber the seat number
    * @return a mounted weapon by index, or `None` if either the seat doesn't exist or there is no controlled weapon
    */
  def WeaponControlledFromSeat(seatNumber : Int) : Seq[Equipment] = {
    Seat(seatNumber) match {
      case Some(seat) =>
        wepFromSeat(seat)
      case None =>
        Seq.empty
    }
  }

  private def wepFromSeat(seat : Chair) : Seq[Equipment] =
    ControlledWeapon(seat.ControlledWeapon.toSeq).getOrElse(Seq.empty)

  def ControlledWeapon(wepNumber : Seq[Int]) : Option[Seq[Equipment]]

  def ControlledWeapon(wepNumber : Int) : Option[Equipment]
}