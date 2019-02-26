// Copyright (c) 2017 PSForever
package net.psforever.objects.ballistics

import net.psforever.objects.vital.DamageResistanceModel
import net.psforever.types.Vector3

/**
  * An encapsulation of a projectile event that records sufficient historical information
  * about the interaction of weapons discharge and a target
  * to the point that the original event might be reconstructed.
  * Reenacting the calculations of this entry should always produce the same values.
  * @param resolution how the projectile hit was executed
  * @param projectile the original projectile
  * @param target what the projectile hit
  * @param damage_model the kind of damage model to which the `target` is/was subject
  * @param hit_pos where the projectile hit
  * @param hit_time the sequence timing when the projectile hit the target
  */
final case class BallisticsInteraction(resolution : ProjectileResolution.Value,
                                       projectile : Projectile,
                                       target : SourceEntry,
                                       damage_model : DamageResistanceModel,
                                       hit_pos : Vector3,
                                       hit_time : Long = System.nanoTime) {
  /**
    * Reference to the data produced from the damage calculations,
    * produced by preparing a curried function to apply the data to a target object,
    * but without actually having to apply that data to some object as a blind test.
    * Known and expected internal data types are `Int` and `Tuple(Int, Int)`.
    */
  private var results : Option[Any] = None
  /**
    * The assignment function for the calculation results data.
    * @see `BallisticsInteraction.setResults`
    */
  private var resFunc : (BallisticsInteraction, Any) => Option[Any] = BallisticsInteraction.setResults

  /**
    * Apply the data to an appropriate results object.
    * @param value the data to set
    * @return the contained `results`
    */
  def Results(value : Any) : Option[Any] = resFunc(this, value)

  /**
    * Display the previously-established appropriate results container object.
    * @return the contained `results`
    */
  def Results : Option[Any] = results
}

object BallisticsInteraction {
  /**
    * The value of the `BallisticInteraction` object's results field may only be set once.
    * This is enforced by changing the function that is used to set the value when setting the value.
    * @param binter the `BallisticInteraction` object
    * @param value the data to set
    * @return the data that was set
    */
  private def setResults(binter : BallisticsInteraction, value : Any) : Option[Any] = {
    binter.results = Some(value)
    binter.resFunc = reportResultsOnly //results only set once
    binter.Results
  }

  /**
    * The value of the `BallisticInteraction` object's results field may only be set once.
    * Once it has been set, the only option is to return that value.
    * @param binter the `BallisticInteraction` object
    * @param value the data to set
    * @return the data that was set
    */
  private def reportResultsOnly(binter : BallisticsInteraction, value : Any) : Option[Any] = binter.Results
}
