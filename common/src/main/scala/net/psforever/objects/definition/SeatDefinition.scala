// Copyright (c) 2017 PSForever
package net.psforever.objects.definition

import net.psforever.objects.vehicles.SeatArmorRestriction

/**
  * The definition for a seat.
  */
class SeatDefinition extends BasicDefinition {
  /** a restriction on the type of exo-suit a person can wear */
  private var armorRestriction : SeatArmorRestriction.Value = SeatArmorRestriction.NoMax
  /** the user can escape while the vehicle is moving */
  private var bailable : Boolean = false
  /** any controlled weapon */
  private var weaponMount : Option[Set[Int]] = None
  Name = "seat"

  def ArmorRestriction : SeatArmorRestriction.Value = {
    armorRestriction
  }

  def ArmorRestriction_=(restriction : SeatArmorRestriction.Value) : SeatArmorRestriction.Value = {
    armorRestriction = restriction
    restriction
  }

  def Bailable : Boolean = {
    this.bailable
  }

  def Bailable_=(canBail : Boolean) : Boolean = {
    bailable = canBail
    canBail
  }

  def ControlledWeapon : Set[Int] = {
    weaponMount.getOrElse(Set.empty[Int])
  }

  def ControlledWeapon_=(wep : Int) : Set[Int] = {
    ControlledWeapon_=(Set(wep))
  }

  def ControlledWeapon_=(wep : Set[Int]) : Set[Int] = {
    weaponMount = Some(wep)
    ControlledWeapon
  }
}
