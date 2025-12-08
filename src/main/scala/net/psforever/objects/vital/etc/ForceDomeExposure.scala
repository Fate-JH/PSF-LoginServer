// Copyright (c) 2025 PSForever
package net.psforever.objects.vital.etc

import net.psforever.objects.sourcing.{AmenitySource, SourceEntry}
import net.psforever.objects.vital.{NoResistanceSelection, SimpleResolutions}
import net.psforever.objects.vital.base.{DamageReason, DamageResolution}
import net.psforever.objects.vital.damage.DamageCalculations
import net.psforever.objects.vital.prop.DamageProperties
import net.psforever.objects.vital.resolution.{DamageAndResistance, DamageResistanceModel}

/**
 * A wrapper for a "damage source" in damage calculations that indicates a harmful interaction from a capitol force dome.
 * @param field the target of the field in question
 */
final case class ForceDomeExposure(field: SourceEntry)
  extends DamageReason {
  def resolution: DamageResolution.Value = DamageResolution.Collision

  def same(test: DamageReason): Boolean = test match {
    case eer: ForceDomeExposure => eer.field eq field
    case _                          => false
  }

  /**
   * Want to blame the capitol facility that is being protected.
   */
  override def attribution: Int = field match {
    case a: AmenitySource => a.installation.Definition.ObjectId
    case _ => field.Definition.ObjectId
  }

  /**
   * A direct connection to the damage information, numbers and properties.
   */
  override def source: DamageProperties = ForceDomeExposure.damageProperties

  /**
   * The functionality that is necessary for interaction of a vital game object with the rest of the hostile game world.
   */
  override def damageModel: DamageAndResistance = ForceDomeExposure.drm

  /**
   * The person to be blamed for this.
   */
  override def adversary: Option[SourceEntry] = None
}

object ForceDomeExposure {
  final val drm = new DamageResistanceModel {
    DamageUsing = DamageCalculations.AgainstExoSuit
    ResistUsing = NoResistanceSelection
    Model = SimpleResolutions.calculate
  }

  final val damageProperties = new DamageProperties {
    Damage0 = 99999
    DamageToHealthOnly = true
    DamageToVehicleOnly = true
    DamageToBattleframeOnly = true
  }
}

