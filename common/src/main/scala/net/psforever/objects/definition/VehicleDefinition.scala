// Copyright (c) 2017 PSForever
package net.psforever.objects.definition

import net.psforever.objects.definition.converter.VehicleConverter
import net.psforever.objects.equipment.EquipmentSize
import net.psforever.objects.inventory.InventoryTile
import net.psforever.objects.vehicles.{DestroyedVehicle, UtilityType}
import net.psforever.objects.vital._
import net.psforever.objects.vital.resistance.ResistanceProfileMutators
import net.psforever.types.Vector3

import scala.collection.mutable
import scala.concurrent.duration._

/**
  * An object definition system used to construct and retain the parameters of various vehicles.
  * @param objectId the object id that is associated with this sort of `Vehicle`
  */
class VehicleDefinition(objectId : Int) extends ObjectDefinition(objectId)
  with ResistanceProfileMutators
  with DamageResistanceModel {
  /** how many points of health the vehicle starts with (should default to maximum if unset) */
  private var defaultHealth : Option[Int] = None
  /** to how many points a damaged vehicle can be repaired */
  private var maxHealth : Int = 100
  /** how many points of shield the vehicle starts with (should default to maximum if unset) */
  private var defaultShields : Option[Int] = None
  /** vehicle shields offered through amp station facility benefits (generally: 20% of health + 1) */
  private var maxShields : Int = 0
  /** the minimum amount of time that must elapse in between distinct shield charge activities (ms) */
  private var shieldChargeDamageCooldown : Long = 0L
  /** the minimum amount of time that must elapse in between damage and shield charge activities (ms) */
  private var shieldChargePeriodicCooldown : Long = 0L
  /** if the shield recharges on its own, this vsalue will be non-`None` and indicate by how much */
  private var autoShieldRecharge : Option[Int] = None
  /* key - seat index, value - seat object */
  private val seats : mutable.HashMap[Int, SeatDefinition] = mutable.HashMap[Int, SeatDefinition]()
  private val cargo : mutable.HashMap[Int, CargoDefinition] = mutable.HashMap[Int, CargoDefinition]()
  /* key - entry point index, value - seat index */
  private val mountPoints : mutable.HashMap[Int, Int] = mutable.HashMap()
  /* seat index (where a weapon attaches during object construction) */
  private val weaponSlots : mutable.Set[Int] = mutable.Set[Int]()
  /* key - seat index (where a weapon attaches during object construction), value - the equipment size of this slot */
  private val weaponSlotSizes : mutable.HashMap[Int, EquipmentSize.Value] = mutable.HashMap[Int, EquipmentSize.Value]()
  /* key - seat index (where a weapon attaches during object construction), value - a weapon definition */
  private val weaponDefaults : mutable.HashMap[Int, ToolDefinition] = mutable.HashMap[Int, ToolDefinition]()
  private var deployment : Boolean = false
  private val utilities : mutable.HashMap[Int, UtilityType.Value] = mutable.HashMap()
  private val utilityOffsets : mutable.HashMap[Int, Vector3] = mutable.HashMap()
  private var deploymentTime_Deploy : Int = 0 //ms
  private var deploymentTime_Undeploy : Int = 0 //ms
  private var trunkSize : InventoryTile = InventoryTile.None
  private var trunkOffset : Int = 0
  private var canCloak : Boolean = false
  private var canBeOwned : Boolean = true
  private var serverVehicleOverrideSpeeds : (Int, Int) = (0, 0)
  private var deconTime : Option[FiniteDuration] = None
  private var maximumCapacitor : Int = 0
  private var destroyedModel : Option[DestroyedVehicle.Value] = None
  Name = "vehicle"
  Packet = VehicleDefinition.converter
  Damage = StandardVehicleDamage
  Resistance = StandardVehicleResistance
  Model = StandardResolutions.Vehicle

  def DefaultHealth : Option[Int] = defaultHealth

  def DefaultHealth_=(health : Int) : Option[Int] = DefaultHealth_=(Some(health))

  def DefaultHealth_=(health : Option[Int]) : Option[Int] = {
    defaultHealth = health
    DefaultHealth
  }

  def MaxHealth : Int = maxHealth

  def MaxHealth_=(health : Int) : Int = {
    maxHealth = health
    MaxHealth
  }

  def DefaultShields : Option[Int] = defaultShields

  def DefaultShields_=(shield : Int) : Option[Int] = DefaultShields_=(Some(shield))

  def DefaultShields_=(shield : Option[Int]) : Option[Int] = {
    defaultShields = shield
    DefaultShields
  }

  def MaxShields : Int = maxShields

  def MaxShields_=(shields : Int) : Int = {
    maxShields = shields
    MaxShields
  }

  def ShieldPeriodicDelay : Long = shieldChargePeriodicCooldown

  def ShieldPeriodicDelay_=(cooldown : Long) : Long = {
    shieldChargePeriodicCooldown = cooldown
    ShieldPeriodicDelay
  }

  def ShieldDamageDelay : Long = shieldChargeDamageCooldown

  def ShieldDamageDelay_=(cooldown : Long) : Long = {
    shieldChargeDamageCooldown = cooldown
    ShieldDamageDelay
  }

  def ShieldAutoRecharge : Option[Int] = autoShieldRecharge

  def ShieldAutoRecharge_=(charge : Int) : Option[Int] = ShieldAutoRecharge_=(Some(charge))

  def ShieldAutoRecharge_=(charge : Option[Int]) : Option[Int] = {
    autoShieldRecharge = charge
    ShieldAutoRecharge
  }

  def Seats : mutable.HashMap[Int, SeatDefinition] = seats

  def Cargo : mutable.HashMap[Int, CargoDefinition] = cargo

  def MountPoints : mutable.HashMap[Int, Int] = mountPoints

  def CanBeOwned : Boolean = canBeOwned

  def CanBeOwned_=(ownable : Boolean) : Boolean =  {
    canBeOwned = ownable
    CanBeOwned
  }

  def CanCloak : Boolean = canCloak

  def CanCloak_=(cloakable : Boolean) : Boolean =  {
    canCloak = cloakable
    CanCloak
  }

  def Weapons : mutable.HashMap[Int, ToolDefinition] = weaponDefaults

  def Weapons(kv : (Int, ToolDefinition)) : mutable.HashMap[Int, ToolDefinition] = {
    val (slot, d) = kv
    weaponSlots += slot
    weaponSlotSizes += slot -> d.Size
    weaponDefaults += kv
    weaponDefaults
  }

  def WeaponSlots : mutable.Set[Int] = weaponSlots

  def WeaponSlots(slot : Int) : mutable.Set[Int] = {
    if(!weaponSlots.contains(slot)) {
      weaponSlots += slot
      weaponSlotSizes += slot -> EquipmentSize.VehicleWeapon
    }
    weaponSlots
  }

  def WeaponSlotSizes : mutable.HashMap[Int, EquipmentSize.Value] = weaponSlotSizes

  def WeaponSlotSizes(kv : (Int, EquipmentSize.Value)) : mutable.HashMap[Int, EquipmentSize.Value] = {
    val (slot, _) = kv
    if(!weaponDefaults.contains(slot) && !weaponSlots.contains(slot)) {
      weaponSlots += slot
      weaponSlotSizes += kv
    }
    weaponSlotSizes
  }

  def Deployment : Boolean = deployment

  def Deployment_=(deployable : Boolean) : Boolean = {
    deployment = deployable
    Deployment
  }

  def Utilities : mutable.HashMap[Int, UtilityType.Value] = utilities

  def UtilityOffset : mutable.HashMap[Int, Vector3] = utilityOffsets

  def DeployTime : Int = deploymentTime_Deploy

  def DeployTime_=(dtime : Int) : Int =  {
    deploymentTime_Deploy = dtime
    DeployTime
  }

  def DeconstructionTime : Option[FiniteDuration] = deconTime

  def DeconstructionTime_=(time : FiniteDuration) : Option[FiniteDuration] = {
    deconTime_=(Some(time))
    DeconstructionTime
  }

  def DeconstructionTime_=(time : Option[FiniteDuration]) : Option[FiniteDuration] = {
    deconTime = time
    DeconstructionTime
  }

  def UndeployTime : Int = deploymentTime_Undeploy

  def UndeployTime_=(dtime : Int) : Int =  {
    deploymentTime_Undeploy = dtime
    UndeployTime
  }

  def TrunkSize : InventoryTile = trunkSize

  def TrunkSize_=(tile : InventoryTile) : InventoryTile = {
    trunkSize = tile
    TrunkSize
  }

  def TrunkOffset : Int = trunkOffset

  def TrunkOffset_=(offset : Int) : Int = {
    trunkOffset = offset
    TrunkOffset
  }

  def AutoPilotSpeeds : (Int, Int) = serverVehicleOverrideSpeeds

  def AutoPilotSpeeds_=(speeds : (Int, Int)) : (Int, Int) = {
    serverVehicleOverrideSpeeds = speeds
    AutoPilotSpeeds
  }

  def AutoPilotSpeed1 : Int = serverVehicleOverrideSpeeds._1

  def AutoPilotSpeed2 : Int = serverVehicleOverrideSpeeds._2

  def MaximumCapacitor : Int = maximumCapacitor

  def MaximumCapacitor_=(maxCapacitor: Int) : Int = {
    maximumCapacitor = maxCapacitor
    MaximumCapacitor
  }

  def DestroyedModel : Option[DestroyedVehicle.Value] = destroyedModel

  def DestroyedModel_=(model : Option[DestroyedVehicle.Value]) : Option[DestroyedVehicle.Value] = {
    destroyedModel = model
    DestroyedModel
  }
}

object VehicleDefinition {
  private val converter = new VehicleConverter

  def apply(objectId: Int) : VehicleDefinition = {
    new VehicleDefinition(objectId)
  }
}
