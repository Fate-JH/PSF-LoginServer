// Copyright (c) 2022 PSForever
package net.psforever.objects

import net.psforever.objects.definition.ObjectDefinition
import net.psforever.objects.serverobject.affinity.FactionAffinity
import net.psforever.objects.serverobject.interior.{InteriorAware, Sidedness}
import net.psforever.objects.vital.resolution.{DamageAndResistance, DamageResistanceModel}
import net.psforever.types.{PlanetSideEmpire, PlanetSideGUID, Vector3}

class DummyExplodingEntity(
                            private val obj: PlanetSideGameObject,
                            private val faction: PlanetSideEmpire.Value
                          )
  extends PlanetSideGameObject
  with FactionAffinity
  with InteriorAware {
  override def GUID: PlanetSideGUID = obj.GUID

  override def Position: Vector3 = {
    if (super.Position == Vector3.Zero) {
      obj.Position
    } else {
      super.Position
    }
  }

  override def Orientation: Vector3 = {
    if (super.Orientation == Vector3.Zero) {
      obj.Orientation
    } else {
      super.Orientation
    }
  }

  override def Velocity : Option[Vector3] = {
    super.Velocity.orElse(obj.Velocity)
  }

  def WhichSide_=(thisSide: Sidedness): Sidedness = WhichSide

  def WhichSide: Sidedness = {
    obj match {
      case awareSource: InteriorAware =>
        awareSource.WhichSide
      case _ =>
        Sidedness.StrictlyBetweenSides
    }
  }

  def Faction: PlanetSideEmpire.Value = faction

  def DamageModel: DamageAndResistance = DummyExplodingEntity.DefaultDamageResistanceModel

  def Definition: ObjectDefinition = obj.Definition
}

object DummyExplodingEntity {
  final val DefaultDamageResistanceModel = new DamageResistanceModel { }

  def apply(obj: PlanetSideGameObject): DummyExplodingEntity = new DummyExplodingEntity(obj, PlanetSideEmpire.NEUTRAL)
}
