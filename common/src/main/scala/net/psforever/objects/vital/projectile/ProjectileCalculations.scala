// Copyright (c) 2017 PSForever
package net.psforever.objects.vital.projectile

import net.psforever.objects.ballistics.BallisticsInteraction

/**
  * The base for all projectile-induced damage calculation function literals.
  */
trait ProjectileCalculations {
  /**
    * The exposed entry for the calculation function literal defined by this base.
    * @param data the historical `BallisticsInteraction` information
    * @return the calculated value
    */
  def Calculate(data : BallisticsInteraction) : Int
}

object ProjectileCalculations {
  type Form = BallisticsInteraction=>Int
}
