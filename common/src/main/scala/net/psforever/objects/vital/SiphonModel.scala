// Copyright (c) 2019 PSForever
package net.psforever.objects.vital

import net.psforever.objects.vital.damage.{DamageCalculations, DamageSelection}
import net.psforever.objects.vital.resolution.{DamageResistCalculations, ResolutionCalculations}

/**
  * Damage calculations for a very specific piece of equipment - the battle frame robotics armor siphon.
  * The raw damage calculations function is inlined since it is specific enough,
  * namely, only does `Direct` damage in 35m.
  */
object SiphonDamageCalculations extends DamageCalculations(
  {
    import net.psforever.objects.ballistics.Projectile
    (projectile : Projectile, rawDamage : Int, distance : Float) => {
      if(distance <= projectile.profile.DamageRadius) {
        rawDamage
      }
      else {
        0
      }
    }
  },
  DamageCalculations.UnmodifiedDamage(DamageCalculations.DamageAgainstVehicle),
  DamageCalculations.DistanceBetweenTargetandSource
)

/**
  * Damage selection for a very specific piece of equipment - the battle frame robotics armor siphon.
  * The "projectile" of this unit will only ever do direct damage.
  */
object SiphonDamage extends DamageSelection {
  def Direct = SiphonDamageCalculations.Calculate
  def Splash = NoDamage.Calculate
  def Lash = NoDamage.Calculate
}

/**
  * Damage resolution for a very specific piece of equipment - the battle frame robotics armor siphon.
  * The siphon will only ever affect a target vehicle's health and will do so ignoring any shield.
  * Even though the calculations call out the possibility of resiatnce,
  * effectively, no target is capable of resisting the armor siphon's effects.
  */
object SiphonResolutions extends DamageResistCalculations(
  ResolutionCalculations.VehicleDamageAfterResist,
  ResolutionCalculations.SimpleApplication
)

/**
  * The damage model used to express a very specific piece of equipment - the battle frame robotics armor siphon.
  * The effects of the armor siphon can not be resisted unless the target is perfectly exempt from those effects.
  */
object SiphonModel extends DamageResistanceModel {
  Damage = SiphonDamage
  Model = SiphonResolutions.Calculate
}
