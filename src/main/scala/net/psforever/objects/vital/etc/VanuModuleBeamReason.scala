// Copyright (c) 2026 PSForever
package net.psforever.objects.vital.etc

import net.psforever.objects.serverobject.beam.VanuModuleBeam
import net.psforever.objects.sourcing.SourceEntry
import net.psforever.objects.vital.base.{DamageReason, DamageResolution}
import net.psforever.objects.vital.prop.DamageProperties
import net.psforever.objects.vital.resolution.DamageAndResistance

final case class VanuModuleBeamReason(beam: VanuModuleBeam) extends DamageReason {
  def resolution: DamageResolution.Value = DamageResolution.Hit

  //borrowing fatal damage from SuicideReason
  def source: DamageProperties = SuicideReason.damageProperties

  def same(test: DamageReason): Boolean = {
    test match {
      case other: VanuModuleBeamReason => other.beam.Zone eq beam.Zone
      case _ => false
    }
  }

  //borrowing damage reachability from SuicideReason
  def damageModel: DamageAndResistance = SuicideReason.drm

  override def adversary: Option[SourceEntry] = None

  override def attribution: Int = beam.Definition.ObjectId
}
