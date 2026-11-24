// Copyright (c) 2026 PSForever
package net.psforever.services.local.support

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorContext, ActorRef, Cancellable, Props}
import net.psforever.actors.zone.BuildingActor
import net.psforever.objects.{Default, GlobalDefinitions, Player}
import net.psforever.objects.definition.converter.OCM
import net.psforever.objects.guid.{GUIDTask, StraightforwardTask, TaskBundle, TaskWorkflow}
import net.psforever.objects.serverobject.beam.{VanuModuleBeam, VanuModuleBeamControl}
import net.psforever.objects.serverobject.damage.Damageable
import net.psforever.objects.serverobject.flag.base.CarriableFlag
import net.psforever.objects.serverobject.flag.module.{ModuleChargeState, VanuModule, VanuModuleNode}
import net.psforever.objects.serverobject.structures.Building
import net.psforever.objects.zones.Zone
import net.psforever.packet.game.packets.{GenericObjectActionEnum, ObjectAttachMessage, ObjectDetachMessage, TriggeredSound}
import net.psforever.services.ServiceManager.{Lookup, LookupResult}
import net.psforever.services.base.envelope.{BundledEnvelope, MessageEnvelope}
import net.psforever.services.base.message.{GenericObjectAction, ObjectDelete, PlanetsideAttribute, SelfRespondingEvent, SendResponse}
import net.psforever.services.{ServiceManager, VanuModuleRotationService}
import net.psforever.services.base.{EventServiceSupport, GenericSupportEnvelopeOnly}
import net.psforever.services.local.LocalAction
import net.psforever.types.{PlanetSideEmpire, Vector3}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

case class VanuModuleSupport(zone: Zone)
  extends EventServiceSupport {
  def label: String = "vanuModuleManager"
  def constructor(context: ActorContext): ActorRef = {
    context.actorOf(Props(classOf[VanuModuleManager], zone), name = "VanuModuleManager")
  }
}

final case class VanuModuleEnvelope(supportMessage: VanuModuleManager.Command)
  extends GenericSupportEnvelopeOnly {
  def supportLabel: String = "vanuModuleManager"
}

final case class VanuModuleUpdate(
                                   toZone: Int,
                                   modules: Seq[(CarriableFlag, PlanetSideEmpire.Value, Long, Vector3)]
                                 ) extends SelfRespondingEvent

object VanuModuleManager {
  final val FlagHoverHeight: Vector3 = Vector3.z(value = 1.05f)

  final val GlobalModuleOwnershipCount: scala.collection.concurrent.TrieMap[PlanetSideEmpire.Value, Int] =
    scala.collection.concurrent.TrieMap.from(PlanetSideEmpire.values.map((_, 0)))

  trait Command

  final case class ModuleSpawnerReady(obj: VanuModuleNode) extends Command

  final case class ModuleSpawnerUnready(obj: VanuModuleNode) extends Command

  final case class NewModuleInZone(module: VanuModule, player: Player) extends Command

  final case class TrackModuleInZone(module: VanuModule) extends Command

  final case class StopTrackingModule(module: VanuModule) extends Command

  //final case class ModuleLimitReached(module: VanuModule)

  final case class DisplayWhichModules(channel: String) extends Command

  final case class DropModule(flag: VanuModule) extends Command

  final case class PickupModuleFromCradle(spawner: VanuModuleNode, player: Player) extends Command

  final case class PickUpModule(flag: VanuModule, player: Player) extends Command

  final case class StartChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player) extends Command

  final case class ChargeModule(module: VanuModule) extends Command

  final case class ModuleLost(module: VanuModule) extends Command

  final case class ModuleFacilityLost(facility: Building) extends Command

  final case class InstallModuleIntoCradle(cradle: VanuModuleNode, module: VanuModule, player: Player) extends Command

  final case class FreeModuleFromCradle(cradle: VanuModuleNode) extends Command

  private case object PeriodicModuleOperations extends Command

  /**
   * A temporary module charge state intended to visualize an uncharged Vanu module as charged while in its spawn cradle.
   * If the module is removed from the cradle outside of appropriate mechanisms, it will be known.
   */
  private case object Demo extends ModuleChargeState(value = true)
  /**
   * A temporary module state intended to represent an ongoing process -
   * that of transforming a Vanu module from a `NonPowered` charge state to a `Charged` charge state.
   * @param sequence how long the charging process will occur (seconds)
   * @param beam reference to the Vanu module beam used in the charging process
   * @param player reference to the player responsible for the charging process;
   *               will usually be the same as the module's carrier, unless the module is dropped mid-charge
   * @param previousChargeState before starting this charging state, the last charge state
   */
  private case class Charging(
                               sequence: Int,
                               beam: VanuModuleBeam,
                               player: Player,
                               previousChargeState: ModuleChargeState
                             ) extends ModuleChargeState(value = false)
  /**
   * A temporary state intended to represent the Vanu module's end of life behavior.
   * @param value visual charge state confirmation value;
   *              in this case, it should be set to carry over the value of the module charge state it is replacing
   * @param attempts number of risky actions performed successfully while expired
   * @param soundAlertIn when the next audible indicator of expiration will occur (ticks)
   */
  private case class Expiration(
                                 override val value: Boolean,
                                 attempts: Int = 0,
                                 soundAlertIn: Int = 15
                               ) extends ModuleChargeState(value)

  /* support functions */

  /**
   * For the Vanu modules in a specific zone,
   * create a series of game packets that properly describes the modules
   * and their current relationships and situations in the zone.
   * @see [[ObjectAttachMessage]]
   * @see [[OCM]]
   * @see [[VanuModuleNode.ThatContainsThis]]
   * @param zone zone in which modules are displayed
   * @param channel channel to which to send response
   * @param moduleList Vanu modules
   */
  private def CreateModulesInZone(zone: Zone, channel: String, moduleList: Seq[VanuModule]): Unit = {
    val outputList = moduleList
      .flatMap { flag =>
        val objectCreatePacket = OCM.apply(flag)
        flag.Carrier
          .map(p => ObjectAttachMessage(p.GUID, flag.GUID, Player.SpecialCarrySlot))
          .orElse {
            val results = VanuModuleNode.ThatContainsThis(flag, flag.Target)
            if (results.hasModuleInstalled) {
              results.GetSpawner.map(o => ObjectAttachMessage(o.GUID, flag.GUID, 0))
            } else {
              None
            }
          }
          .map(attachPacket => List(objectCreatePacket, attachPacket))
          .getOrElse(List(objectCreatePacket))
      }
    if (outputList.nonEmpty) {
      zone.LocalEvents ! MessageEnvelope(channel, SendResponse(outputList))
    }
  }

  /**
   * A functional Vanu module cradle can spawn a Vanu module.
   * @param cradle Vanu module cradle
   * @return `true`, if the cradle is viable;
   *         `false`, otherwise
   */
  def CradleIsFunctionalStatus(cradle: VanuModuleNode): Boolean = {
    cradle.Faction != PlanetSideEmpire.NEUTRAL &&
      cradle.Owner.asInstanceOf[Building].CaptureTerminal.forall(_.HackedBy.isEmpty)
  }

  /**
   * To maintain the process of charging an unpowered Vanu module so it may look like a powered module of a certain type,
   * certain criteria involving all parts of the process must be maintained every tick.
   * The carrier must be alive,
   * the module must not be already charged,
   * the module must not be destroyed,
   * the player who the process thinks is the carrier of the module must actually be carrying the module,
   * and the carrier must be within a certain distance of the Vanu module core beam that performs the charging.
   * @param player carrier of the Vanu module
   * @param beam amenity utilized as a medium for charging the Vanu module
   * @param module Vanu module to be charged
   * @return `true`, if the conditions for charging are acceptable for this process tick;
   *         `false`, if the charging process should be aborted
   */
  def PlayerModuleChargingTests(player: Player, beam: VanuModuleBeam, module: VanuModule): Boolean = {
    player.isAlive &&
      !module.Charged.value && !module.Destroyed && module.Carrier.contains(player) &&
      Vector3.DistanceSquared(player.Position.xy, beam.Position.xy) < 2 * math.pow(beam.Definition.UseRadius, 2)
  }

  /**
   * The roaming time is how long the module can remain free from a cradle before it expires.
   * The default should be 15 minutes (900000 milliseconds).
   * If the module has already been at least installed once,
   * it has a known initial installation time,
   * and that will factor into the the ideal roaming time.
   * @param module Vanu module that is going to roam
   * @param suggestedTime recommended time to roam (ms)
   * @return for how long the module may roam unexpired (ms)
   */
  private def GetIdealRoamingTime(module: VanuModule, suggestedTime: Long): Long = {
    if (module.isExpirationDurationSet) {
      math.min(suggestedTime, module.GetRemainingExpirationTime())
    } else {
      suggestedTime
    }
  }

  /**
   * The installation/expiration time is how long the module can exist before it is deconstructed.
   * The default should be 28 hours (100800000 milliseconds) and that is only set once during the first installation.
   * On subsequent installations,
   * if the module is installed while expired,
   * the installation time is set to a minimum of a few minutes, maximum of two hours (7200000).
   * @param module Vanu module that has been installed
   */
  private def SetModuleInstallationTime(module: VanuModule): Unit = {
    val chargeState = module.Charged
    if (chargeState.isInstanceOf[VanuModuleManager.Expiration]) {
      //salvaged expired module
      val VanuModuleManager.Expiration(_, attempts, _) = chargeState
      module.SetExpirationTime(math.max(
        (attempts + 7) * 60000L, /* minimum of a few minutes */
        (module.GetRoamingTime().toDouble * 8d).toLong /* maximum of 2 hours */
      ))
    } else if (!module.isExpirationDurationSet) {
      //only set the first installation; lasts until expiration, or the end of module's life
      module.SetExpirationTime(durationTime = 100800000L) /* 28 hours */
    }
  }

  /**
   * How many modules does a given empire claim as their own, under their control and offering their soldiers benefits?
   * @param faction module-owning empire
   * @return number of Vanu modules this empire claims ownership of
   */
  def NumberOfModulesFactionOwns(faction: PlanetSideEmpire.Value): Int = {
    GlobalModuleOwnershipCount(faction)
  }

  /**
   * Empires have a limited number of Vanu modules they can own on an interstellar scale.
   * If they own the maximum number or modules or have stolen more
   * they can not claim ownership of any more legitimately.
   * @param faction module-owning empire
   * @return `true`, if it can claim ownership to more Vanu modules;
   *         `false`, otherwise
   */
  def FactionCanOwnMoreModules(faction: PlanetSideEmpire.Value): Boolean = {
    // 10 is hardcoded because the wiki says so
    GlobalModuleOwnershipCount(faction) < 10
  }

  /**
   * An empire can claim ownership of a Vanu module
   * only if they have not yet installed the maximum number of Vanu modules legitimately
   * or as many as they want if that Vanu module was stolen from an enemy faction's cradle.
   * @param faction module-owning empire
   * @param moduleFaction empire the module thinks it belongs to
   * @return `true`, if that empire can claim they own another Vanu module;
   *         `false`, otherwise
   */
  private def TryGiveFactionModuleOwnership(
                                             faction: PlanetSideEmpire.Value,
                                             moduleFaction: PlanetSideEmpire.Value
                                           ): Boolean = {
    if (FactionCanOwnMoreModules(faction) || faction != moduleFaction) {
      GlobalModuleOwnershipCount.update(faction, GlobalModuleOwnershipCount(faction) + 1)
      true
    } else {
      false
    }
  }

  /**
   * An empire can always give up claims of ownership of a Vanu module.
   * @param faction module-owning empire
   */
  private def RemoveModuleFromFactionOwnership(faction: PlanetSideEmpire.Value): Unit = {
    val updatedCount = math.max(GlobalModuleOwnershipCount(faction) - 1, 0)
    GlobalModuleOwnershipCount.update(faction, updatedCount)
  }

  /**
   * Register a Vanu module for a globally unique identifier and
   * if that registration succeeds
   * perform a canned action upon the newly spawned module.
   * @param zone zone in which the module will be registered
   * @param module Vanu module
   * @param behavior action that is performed upon the Vanu module once it is registered
   * @param extraContext how to describe what the extra action on the registered Vanu module;
   *                     a leading space or leading punctuation mark for formatting is recommended
   * @return compiled registration then action tasking
   */
  private def VanuModuleRegistrationBeforeTask(
                                                zone: Zone,
                                                module: VanuModule,
                                                behavior: (Zone, VanuModule) => Unit,
                                                extraContext: String = ""
                                              ): TaskBundle = {
    TaskBundle(
      new StraightforwardTask() {
        private val localFlag = module
        private val localZone = zone
        private val localFunc = behavior

        override def description(): String = s"register a ${module.ValidFlagType} Vanu module$extraContext"

        def action(): Future[Any] = {
          localFunc(localZone, localFlag)
          Future(true)
        }
      },
      List(GUIDTask.registerObject(zone.GUID, module))
    )
  }
}

/**
 * Central authority script that controls the lifecycle of Vanu modules for  particular zone.<br>
 * <br>
 * The manager is not draconic.
 * It will not look at some independent Vanu module in the world and treat it like a foreign concept.
 * If such an independent Vanu module existed in the game world,
 * it could easily be associated with the players who would carry it,
 * with the receptacles that would have the module installed in it,
 * and be picked up and put down, all working just fine.
 * Most of the routine functionality of the modules are designed in a purely transactional sort of way
 * where no being formally associated with the manager does not mean the manager won't sort it fairly.
 * The manager's biggest purposes that actually require formal acknowledgment of the modules
 * (which has to be activated, that is, opted-in)
 * involve the Vanu module lifecycle
 * and the display of Vanu module icons on the continental maps.
 * In a sense, however, the management of Vanu module icons is merely facilitated delivery by the manager.
 * The majority of its presentation is operated by the client UI
 * through the use of a `VanuModuleUpdateMessage` packet.
 * It's the Vanu module lifecycle that is whole controlled through the manager.<br>
 * <br>
 * <h2>VANU MODULE LIFECYCLE</h2>
 * The lifecycle of the Vanu module is facilitated through this manager via three or four mechanisms,
 * three of them being internal module flags and timers.
 * A much more global script than even the manager causes rotation of the modules,
 * a concatenation of the beginning and end of a module's existence.
 * The module's charged state defines how the manager interacts with it and where in the lifecycle the module is positioned.
 * Two timers on the module use its charged state as context as how to interpret that charged state
 * and bind one state transitioning to the next.<br>
 * <br>
 * <h3>DEMO</h3>
 * All modules start as constructed in their appropriate spawners during a module rotation event.
 * The modules are marked as DEMO which means they are seen as being CHARGED,
 * revealing the characteristic color of their pulsation which reveals their functionality.
 * If the module is not removed from the spawning cradle before the next module rotation event,
 * it will be deconstructed quietly.<br>
 * <br>
 * <h3>NONPOWERED</h3>
 * Once a player removes it from the spawning cradle and is holding it,
 * the module will be switched from being DEMO to be NONPOWERED,
 * a state that conceals the nature of the module with an off-white halation effect.
 * The module is assigned a roaming timer that determines how long it can survive outside of a cradle in the caverns.
 * If a NONPOWERED module attempts to be removed from the cavern, it will be quietly deconstructed in a denied transfer.<br>
 * <br>
 * <h3>EXPIRATION</h3>
 * If allowed to persist outside of a cradle long enough, the module will switch from a NONPOWERED state to an EXPIRATION state.
 * The expiration timer will be set and the module will beep periodically, indicating the EXPIRATION status.
 * At this point, various actions that can be performed on the module have a chance of causing the module to explode.
 * While the explosion is on an inevitable timer,
 * the chance of the module exploding before that increases the closer it is to exploding
 * and the number of attempted risky actions that did not cause the module to explode.
 * Damageable things near the module will be damaged in the explosion, of course, with players dying from it.<br>
 * <br>
 * <h3>CHARGING, CHARGED</h3>
 * Taking the module to a staple of the cavern zone in which they are typically spawned - the cavern core beam -
 * states a smaller loop process that transforms the NONPOWERED state or EXPIRATION state module to start CHARGING.
 * After a time, the module will transition from the CHARGING state to a CHARGED state.
 * The true functionality of the module, concealed by being NONPOWERED, will be revealed again in the CHARGED state
 * and the concealing off-white halation effect will yield to a colored halation characteristic to the module.
 * The CHARGED module continues to use whatever roaming time remains on its clock while it stays in the same cavern.
 * If it is removed from the cavern in this state, and it can switch zones safely in this state,
 * then the roaming time is set to a fully renewed time.<br>
 * <br>
 * If an EXPIRATION state module transitions to CHARGED through the aforementioned process,
 * it will continue to use the abbreviated EXPIRATION roaming time until it is transported out of the spawning zone.
 * It will transition back into a renewed EXPIRATION state if that timer expires;
 * but, as it has already been CHARGED once, it can not undergo the charging process a second time.<br>
 * <br>
 * <h3>INSTALLED</h3>
 * Once the module has escaped the spawning cavern,
 * it's roaming time is renewed,
 * and it must be taken to a major facility where it is to be installed into an available module receptacle in the lobby.
 * The first time the module is installed, it is assigned a maximum installation time and its roaming time is disabled.
 * It will now grant cavern benefits to that major facility and all lattice-connected major facilities.
 * It will not regain roaming time unless it is removed again from the cradle.
 * Every subsequent cradle installation reuses that same installation expiration timer,
 * giving the module a maximum lifespan from this point on.
 * It will not lose installation expiration time unless it becomes roaming,
 * it expires while roaming,
 * then is either reinstalled into another cradle or explodes.<br>
 * <br>
 * When placed into that lobby cradle, it will be locked into the said cradle and can not be removed casually
 * save for situations where the facility is hacked.
 * When the facility is hacked, the module's benefits will be muted but it will remain installed.
 * When hacked it becomes available for being removed the cradle, either for safety or through theft.
 * If the facility formally changes factional hands, ore loses power, the modules are quietly deconstructed.
 * If the installation expiration time passes,
 * the module will quietly deconstruct if installed or shift back to a normal EXPIRATION state if uninstalled.<br>
 * <br>
 * <h3>END</h3>
 * As mentioned above, a module will end its existence in one of two ways as per its lifecycle.
 * After being installed for too long, the module will deconstruct quietly.
 * After being uninstalled for too long, the module will explode violently.
 * @param zone zone in which these recognized Vanu modules are processed
 */
class VanuModuleManager(zone: Zone) extends Actor {
  import VanuModuleManager._

  /** list of Vanu module spawners */
  private var cradles: Seq[VanuModuleNode] = Seq()
  /** list of Vanu modules whose lifecycle is managed by this zone */
  private var trackedModules: Seq[VanuModule] = Seq()
  /** self-messaging timer for the Vanu module lifecycle */
  private var vanuModuleUpdateTimer: Cancellable = Default.Cancellable
  /** system event bus for sending Vanu module icon updates */
  private var galaxyService: ActorRef = Default.Actor

  override def preStart(): Unit = {
    super.preStart()
    ServiceManager.serviceManager ! Lookup("galaxy")
  }

  /* vanu module manager behavior patterns */

  def receive: Receive = setupBehaviorGalaxy

  private val AddRemoveCradles: Receive = {
    case ModuleSpawnerReady(spawner) =>
      addSpawner(spawner)

    case ModuleSpawnerUnready(spawner) =>
      removeSpawner(spawner)
  }

  private val setupBehaviorGalaxy: Receive = AddRemoveCradles.orElse {
    case LookupResult("galaxy", endpoint) =>
      galaxyService = endpoint
      context.become(setupBehaviorModuleRotation)
      ServiceManager.receptionist ! Receptionist.Find(
        VanuModuleRotationService.VanuModuleRotationServiceKey,
        context.self.toTyped
      )

    case _ => ()
  }

  private val setupBehaviorModuleRotation: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.VanuModuleRotationServiceKey.Listing(listings) =>
      val manager = listings.head
      manager ! VanuModuleRotationService.KeepUpdated(self)
      context.become(setInitialSpawners)

    case _ => ()
  }

  private val setInitialSpawners: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.CurrentModulesAvailable(currentlyAvailableModules) =>
      cradles
        .filter(o => currentlyAvailableModules.contains(o.ValidFlagType))
        .foreach(trySpawnModuleInCradle)
      context.become(standardRotation)

    case _ => ()
  }

  private val standardRotation: Receive = AddRemoveCradles.orElse {
    case VanuModuleRotationService.RotationHasOccurred(previousModules, currentlyAvailableModules) =>
      rotationHasOccurred(previousModules, currentlyAvailableModules)

    case NewModuleInZone(module, host) =>
      newModuleInZone(module, host)

    case TrackModuleInZone(module) =>
      trackModule(module)

    case StopTrackingModule(module) =>
      untrackModule(module)

    case DisplayWhichModules(channel) =>
      CreateModulesInZone(zone, channel, trackedModules.filterNot(_.Destroyed))

    case PeriodicModuleOperations =>
      periodicModuleOperations()

    case PickupModuleFromCradle(spawner, player) =>
      pickupModuleFromCradle(spawner, player)

    case DropModule(flag) =>
      dropModule(flag)

    case PickUpModule(flag, player) =>
      pickUpModule(flag, player)

    case StartChargingModule(module, beam, player) =>
      startChargingModule(module, beam, player)

    case ChargeModule(flag) =>
      chargeModule(flag)

    case InstallModuleIntoCradle(cradle, flag, player) =>
      installModuleIntoCradle(flag, cradle, player)

    case ModuleLost(flag) =>
      startModuleCleanup(flag)

    case ModuleFacilityLost(facility) =>
      moduleFacilityLost(facility)

    case FreeModuleFromCradle(cradle) =>
      freeModuleFromCradle(cradle)

    case _ => ()
  }

  /* vanu module receptacle support */

  /**
   * A Vanu module spawner is being introduced to this manager.
   * If there is a module already in the spawning receptacle, track it.
   * @param spawner Vanu module receptacle
   */
  private def addSpawner(spawner: VanuModuleNode): Unit = {
    val spawnerGuid = spawner.GUID
    cradles
      .find(_.GUID == spawnerGuid)
      .getOrElse {
        cradles = cradles :+ spawner
        spawner.captureFlag.collect { case o: VanuModule => trackModule(o) }
      }
  }

  /**
   * A Vanu module spawner is asked to be forgotten by this manager.
   * If there is a module in the spawning receptacle at the time, stop tracking it.
   * @param spawner Vanu module receptacle
   */
  private def removeSpawner(spawner: VanuModuleNode): Unit = {
    val spawnerGuid = spawner.GUID
    cradles.indexWhere(_.GUID == spawnerGuid) match {
      case -1 => ()
      case index =>
        cradles = cradles.take(index) ++ cradles.drop(index + 1)
        spawner.captureFlag.collect { case o: VanuModule => untrackModule(o) }
    }
  }

  /* general vanu module manager support operations, including module lifecycle */

  /**
   * Introduce a Vanu module into the manager's Vanu module lifecycle routine.
   * This will make it respect imposed state changes based on its internal timers and charge flags
   * and it will be displayed as an icon on the continental map.
   * Once there is even a single module to be tracked, start the periodic timer.
   * @param module Vanu module to be included
   * @return always `true`;
   *         either it is a new module to be tracked, or an existing module that is already tracked
   *         corresponds to reasoning of `Boolean` return value from `untrackModule`
   */
  private def trackModule(module: VanuModule): Boolean = {
    trackedModules.indexWhere(flag => module eq flag) match {
      case -1 =>
        trackedModules = trackedModules :+ module
        if (vanuModuleUpdateTimer.isCancelled) {
          //see periodicModuleOperations for where the display timer is cancelled
          vanuModuleUpdateTimer = context.system.scheduler.scheduleAtFixedRate(
            initialDelay = 1000 milliseconds,
            interval = 1000 milliseconds,
            self,
            PeriodicModuleOperations
          )
        }
      case _ => ()
    }
    true
  }

  /**
   * Exclude a Vanu module from the manager's Vanu module lifecycle routine.
   * @param module Vanu module to be included
   * @return `true` if the module was removed;
   *         `false` otherwise
   */
  private def untrackModule(module: VanuModule): Boolean = {
    trackedModules.indexWhere(flag => module eq flag) match {
      case -1 =>
        false
      case index =>
        //see periodicModuleOperations for where the display timer is cancelled
        trackedModules = trackedModules.take(index) ++ trackedModules.drop(index + 1)
        true
    }
  }
  /**
   * Periodic behaviors affecting any Vanu module tracked by this manager.
   * This includes the manager's Vanu module lifecycle routine and displaying the modules on the continental map.
   * If there are no modules to be tracked, cancel the periodic timer.
   */
  private def periodicModuleOperations(): Unit = {
    handleModuleLifecycle()
    updateVanuModuleIcons()
    if (trackedModules.isEmpty) {
      vanuModuleUpdateTimer.cancel()
      vanuModuleUpdateTimer = Default.Cancellable
    }
  }

  /**
   * Examine all known Vanu modules in a give zone and determine their current life trajectory or their end-of-life status.
   * This is an expression of the natural course of a module's behaviors.
   * Over-all, there are three independent progressions to the module: charging, expiring, existing.
   */
  private def handleModuleLifecycle(): Unit = {
    /*
    This Vanu module lifecycle is imposed upon all tracked modules.
    All phases of the lifecycle are controlled by four internal module flags:
    either of two timers, one for roaming and the other for installation/expiration,
    the current charge state,
    and whether or not the module has been `Destroyed`.
    Processes within the greater lifecycle are generally caused by setting the charged state to a specific value
    and one of the timers in such a way that it supports the process.
    EXISTING is the module just being in some receptacle, in someone's hands, or on the ground,
    and not bothering anyone.
    CHARGING is the module being exposed to the Vanu module core beam
    and going from being unpowered to being charged (powered).
    EXPIRING is the module being like EXISTING
    but it has neared the end of its existence and is within a few actions or moments of exploding.
     */
    val (charging, _chargedOrUncharged) = trackedModules.partition(_.Charged.isInstanceOf[VanuModuleManager.Charging])
    val (_expiredSomehow, _haveNotExpired) = _chargedOrUncharged.partition(_.Charged.isInstanceOf[VanuModuleManager.Expiration])
    val (decayingOutsideOfCradle, _functionallyExpired) = _expiredSomehow.partition(_.GetRoamingTime() > 0L)
    val (_shouldHaveExploded, haveNotExplodedYet) = _functionallyExpired.partition(_.Destroyed)
    val (lingeringAfterExplosion, explodedReadyForCleanup) = _shouldHaveExploded.partition(_.GetRemainingExpirationTime() > 0L)
    val (_ongoing, outOfRoamingTime) = _haveNotExpired.partition(_.GetRoamingTime() > 0L)
    val (installedAndStable, _outOfInstallationTime) = _ongoing.partition(_.GetRemainingExpirationTime() > 0L)
    val (despawnWhenInstalled, explodeWhenNotInstalled) = _outOfInstallationTime.partition(_.isInstalled)
    val modulesToBeArmed = outOfRoamingTime ++ explodeWhenNotInstalled
    trackedModules = charging ++ decayingOutsideOfCradle ++ haveNotExplodedYet ++ lingeringAfterExplosion ++ modulesToBeArmed ++ installedAndStable
    //charging modules
    charging.foreach(periodicChargingEvent)
    //modules that just vanish
    (explodedReadyForCleanup ++ despawnWhenInstalled).foreach(startModuleCleanup)
    //modules that explode (not been installed in a cradle recently)
    haveNotExplodedYet.foreach(explodeModule)
    //modules that make an audible sound to indicate their instability
    decayingOutsideOfCradle.foreach(alertToExpiredStatus)
    //modules that will explode later (too long outside of a cradle)
    modulesToBeArmed.foreach(expireTheModule)
  }

  /**
   * Charge a Vanu module, usually in a cavern core beam, such that it goes from being unpowered to being charged (powered).
   * Charging a module is its own miniature lifecycle within the over-all module lifecycle.
   * While it is ongoing, timers belonging to the over-all lifecycle of the module are still counting down,
   * but the consequences of those timers will not be visible until the charging process has been exited.
   * The module must be carried by a player to complete this charging lifecycle correctly.
   * @param module Vanu module being charged
   */
  private def periodicChargingEvent(module: VanuModule): Unit = {
    val VanuModuleManager.Charging(sequence, beam, player, previous) = module.Charged
    module.Carrier match {
      case None =>
        //not being carried anymore - stop
        stopChargingModuleBasics(module)
        beam.Actor ! VanuModuleBeamControl.ChargeEnd(player, module)
      case Some(carrier)
        if sequence <= 0 && VanuModuleManager.PlayerModuleChargingTests(carrier, beam, module) =>
        //charge complete
        chargeComplete(module, beam, carrier)
        beam.Actor ! VanuModuleBeamControl.ChargeComplete(carrier, module)
      case Some(carrier)
        if sequence > 0 && VanuModuleManager.PlayerModuleChargingTests(carrier, beam, module) =>
        //continue charging process ...
        module.Charged = VanuModuleManager.Charging(sequence - 1, beam, carrier, previous)
      case Some(player) =>
        //conditions where charge should be stopped (process failed?)
        stopChargingModule(module, player)
        beam.Actor ! VanuModuleBeamControl.ChargeFailed(player, module)
    }
  }

  /**
   * Instruct the Vanu module that it has passed its expiration time and must promptly explode.
   * Afterward, the module will briefly appear in the zone in an invalid state before being properly deconstructed.
   * @param module Vanu module to explode
   */
  private def explodeModule(module: VanuModule): Unit = {
    module.SetRoamingTime(Long.MaxValue)
    module.SetExpirationTime(durationTime = 2000L) //will vanish after this
    module.Destroyed = true //set module to invalid
    module.Carrier match {
      case Some(_) =>
        dropModule(module)
      case None =>
        zone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(OCM.apply(module))) //spawn invalid flag that explodes
    }
    Zone.serverSideDamage(
      zone,
      module,
      Zone.explosionDamage(None, module.Position)
    )
  }

  /**
   * When a module is expired, every so often, it will release an alarm jingle that indicates it is expired.
   * If it alarms, you should feel alarmed.
   * @param module Vanu module that beeps
   */
  private def alertToExpiredStatus(module: VanuModule): Unit = {
    val VanuModuleManager.Expiration(state, attempts, soundAlertIn) = module.Charged
    if (soundAlertIn == 0) {
      zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.TriggerSound(TriggeredSound.ModuleAlarmBeep, module.Position, 15, volume = 1.0f))
      module.Charged = VanuModuleManager.Expiration(state, attempts)
    } else {
      module.Charged = VanuModuleManager.Expiration(state, attempts, soundAlertIn - 1)
    }
  }

  /**
   * After a Vanu module has roamed long enough outside of a valid receptacle, it will expire, which indicates that it will explode eventually.
   * A remaining roaming time is set and the charge state is set to be expired.
   * It will now beep in warning periodically.
   * @param module Vanu module that has expired
   */
  private def expireTheModule(module: VanuModule): Unit = {
    val calculatedExplosionTime = (math.random() * 299001d).toLong + 1000L //between 1s and 5min
    module.SetRoamingTime(GetIdealRoamingTime(module, calculatedExplosionTime))
    module.SetExpirationTime(Long.MaxValue)
    module.Charged = VanuModuleManager.Expiration(module.Charged.value)
    zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.TriggerSound(TriggeredSound.ModuleAlarmBeep, module.Position, 15, volume = 1.0f))
  }

  /**
   * Organize all valid modules in this zone and compose data to be sent to the clients.
   * @see [[net.psforever.packet.game.packets.VanuModuleUpdateMessage]]
   */
  private def updateVanuModuleIcons(): Unit = {
    //VanuModuleUpdateMessage can onlu take a list of 63 module information entries max
    val (roamingModules, installedModules) = trackedModules.filterNot(_.Destroyed).partition(!_.isInstalled)
    val selectedRoamingModules = roamingModules.take(63)
    val selectedInstalledModules = installedModules.take(63 - selectedRoamingModules.size)
    val outputList = (selectedRoamingModules ++ selectedInstalledModules)
      .map { module => (module.DisplayFlagType, module.Faction, 0L, module.Position.xy) }
    galaxyService ! MessageEnvelope("", VanuModuleUpdate(zone.Number, outputList))
  }

  /* pure support functions */

  /**
   * Every zone has a limit to the number of "roaming" modules that are allowed at one time.
   * A roaming module is one that is not in a Vanu module receptacle.
   * It is in someone's hands or it is on the ground.
   * @return
   */
  private def canHaveMoreRoamingModulesInZone: Boolean = {
    trackedModules.count(!_.isInstalled) < zone.Info.moduleRoamingLimit
  }

  /**
   * When a Vanu module has expired, certain player actions that manipulate the module are considered "risky".
   * If a they fail a coin toss, with no additional warning, the module will explode.
   * The following actions are considered risky:
   * dropping the module,
   * picking the module up,
   * installing the module in cradle,
   * and transporting the module between zones.
   * @param module Vane module that is a risk
   * @return `true`, if the action is successful;
   *         `false`, if the action is not successful and the module exploded
   */
  private def succeedAtRiskyAction(module: VanuModule): Boolean = {
    if (module.Charged.isInstanceOf[VanuModuleManager.Expiration]) {
      if (!module.Destroyed) {
        succeedAtRiskyActionBasics(module)
      } else {
        false //module is destroyed; technically an invalid state to reach
      }
    } else {
      true //normal action
    }
  }

  /**
   * When a Vanu module has expired, certain player actions that manipulate the module are considered "risky".
   * If a they fail a coin toss, with no additional warning, the module will explode.
   * If the player is attempting to salvage the module by installing it in a receptacle,
   * but only ten seconds remain before the explosion,
   * let them do it without risk of explosion.
   * They probably went through hell just to get there in time.
   * @param module Vane module that is a risk
   * @return `true`, if the action is successful;
   *         `false`, if the action is not successful and the module exploded
   */
  private def succeedAtRiskyInstallation(module: VanuModule): Boolean = {
    val moduleIsDestroyed = module.Destroyed
    if (module.Charged.isInstanceOf[VanuModuleManager.Expiration] && !moduleIsDestroyed) {
      module.GetRoamingTime() < 10000L || succeedAtRiskyActionBasics(module)
    } else if (moduleIsDestroyed) {
      false //module is destroyed; technically an invalid state to reach
    } else {
      true //normal action
    }
  }

  /**
   * When a Vanu module has expired, certain player actions that manipulate the module are considered "risky".
   * If a they fail a coin toss, with no additional warning, the module will explode.
   * If the risky action can be completed without the module exploding,
   * the module produces a warning beep as is mentioned elsewhere.
   * @param module Vane module that is a risk
   * @return `true`, if the action is successful;
   *         `false`, if the action is not successful and the module exploded
   */
  private def succeedAtRiskyActionBasics(module: VanuModule): Boolean = {
    //ready to explode but not yet exploded
    val VanuModuleManager.Expiration(state, attempts, _) = module.Charged
    val remainingTimeAlive = module.GetRoamingTime().toDouble
    if (math.random() * remainingTimeAlive < 10000d + math.pow(5, attempts + 1)) {
      //explode upon action
      explodeModule(module)
      false
    } else {
      //no explosion; survived compromised action
      zone.LocalEvents ! MessageEnvelope(zone.id, LocalAction.TriggerSound(TriggeredSound.ModuleAlarmBeep, module.Position, 15, volume = 1.0f))
      module.SetRoamingTime(remainingTimeAlive.toLong - attempts.toLong * 1000L)
      module.Charged = VanuModuleManager.Expiration(state, attempts + 1)
      true
    }
  }

  /* vanu module packet handling */

  /**
   * Explicitly cycle which module spawners are active.
   * If the spawner for a certain type of module is cradling a Vanu module and is listed as a "stop",
   * then attempt to deconstruct that Vanu module.
   * If the spawner for a certain type of module is cradling a Vanu module and is listed as a "start",
   * then attempt to spawn that Vanu module.
   * If the spawner for a certain type of module is cradling a Vanu module and
   * is listed as both "stop" and "start", i.e., in both categories,
   * then attempt to deconstruct that Vanu module and then spawn a new Vanu module of that type.
   * @see [[VanuModuleRotationService]]
   * @param stopModules module types that should be active now
   * @param startModules module types that should be activated if they are not
   */
  private def rotationHasOccurred(stopModules: Seq[CarriableFlag], startModules: Seq[CarriableFlag]): Unit = {
    val stopModuleSet = stopModules.toSet
    val startModuleSet = startModules.toSet
    val stopAndStartModules = stopModuleSet.intersect(startModuleSet)
    val modulesToActivate = startModuleSet.diff(stopModuleSet) ++ stopAndStartModules
    val modulesToClear = stopModuleSet.diff(modulesToActivate) ++ stopAndStartModules
    cradles
      .filter(o => modulesToClear.contains(o.ValidFlagType))
      .foreach(tryDespawnModuleAssociatedWithCradle)
    cradles
      .filter(o => modulesToActivate.contains(o.ValidFlagType))
      .foreach(trySpawnModuleInCradle)
  }

  /**
   * Collect a Vanu module from a receptacle and give it to a player to carry it around.
   * Used for both virgin Vanu modules removed from spawners
   * and properly charged Vanu modules removed from installation cradles.
   * When removed from its receptacle, the Vanu module will count as "roaming"
   * and will persist for a roaming time allowance (part of the module lifecycle).
   * If the module receptacle is not a spawning cradle,
   * the module will no longer be considered owned by the faction that owns the receptacle.
   * @param cradle storage receptacle for the Vanu module;
   *               the said Vanu module is already installed in the cradle
   * @param player player that will carry the Vanu module after it has been collected
   */
  private def pickupModuleFromCradle(cradle: VanuModuleNode, player: Player): Unit = {
    cradle.captureFlag match {
      case Some(module: VanuModule)
        if canHaveMoreRoamingModulesInZone =>
        val zoneid = module.Zone.id
        val flagGuid = module.GUID
        val playerPosition = player.Position
        val flagType = module.ValidFlagType
        val flagCategory = flagType.category
        module.SetRoamingTime(GetIdealRoamingTime(module, suggestedTime = 900000L)) //(ms; or, 15 minutes)
        module.Carrier = Some(player)
        module.WhichSide = player.WhichSide
        module.Target = Building.NoBuilding
        if (module.Charged != ModuleChargeState.Charged) {
          module.Charged = ModuleChargeState.NonPowered
        }
        player.Carrying = Some(flagCategory)
        cradle.captureFlag = None
        if (cradle.ValidFlagType != flagType) {
          RemoveModuleFromFactionOwnership(cradle.Faction)
        }
        module.Position = Vector3.Zero //spawn far out of sight, at first
        val flagOcmPacket = OCM.apply(module)
        module.Position = playerPosition
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, playerPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(
            ObjectDetachMessage(cradle.GUID, flagGuid, cradle.Position, 0, 0, 0), /* cradle emits module */
            flagOcmPacket, /* overwrite with concealed module */
            ObjectAttachMessage(player.GUID, flagGuid, Player.SpecialCarrySlot) /* module held by player */
          )),
          MessageEnvelope(player.Name, LocalAction.PlayerCollectsFlag(flagCategory, module.GUID))
        )
      case _ => ()
    }
  }

  /**
   * If a carrier is transporting a Vanu module, they may place in on the ground at their feet.
   * (It will actually hover above the ground at about chest height.)
   * No change to any timers or the operational status of the module happens.
   * This counts as a "risky action".
   * @param module Vanu module to be dropped;
   *               some player is already swerving as the carrier for the said module
   */
  private def dropModule(module: VanuModule): Unit = {
    module.Carrier match {
      case Some(player)
        if succeedAtRiskyAction(module) =>
        val zoneid = zone.id
        val playerPosition = player.Position
        val flagPosition = playerPosition + FlagHoverHeight
        val flagOrientation = player.Orientation
        module.Position = flagPosition
        module.Orientation = flagOrientation
        module.WhichSide = player.WhichSide
        module.Carrier = None
        player.Carrying = None
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, playerPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(player.GUID, module.GUID, flagPosition, 0, 0, flagOrientation.z))),
          MessageEnvelope(player.Name, LocalAction.PlayerDropsFlag(module.GUID))
        )
      case _ => ()
    }
  }

  /**
   * A Vanu module, having previously been dropped on the ground, is collected for carrying by a player.
   * No change to any timers or the operational status of the module happens.
   * This counts as a "risky action".
   * @param module Vanu module found dropped on the ground
   * @param player player who would be the carrier for the Vanu module
   */
  private def pickUpModule(module: VanuModule, player: Player): Unit = {
    module.Carrier match {
      case None
        if succeedAtRiskyAction(module) =>
        val zoneid = module.Zone.id
        val flagCategory = module.ValidFlagType.category
        module.Carrier = Some(player)
        player.Carrying = Some(flagCategory)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModulePickup, player.Position, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(player.GUID, module.GUID, Player.SpecialCarrySlot))),
          MessageEnvelope(player.Name, LocalAction.PlayerCollectsFlag(flagCategory, module.GUID))
        )
      case _ => ()
    }
  }

  /**
   * The start of the process of transforming an unpowered Vanu module into a charged (powered) Vanu Module.
   * The initial charge state of the module is retained but is never polled.
   * As long as it is part of the normal Vanu module lifecycle,
   * the temporary change in module charge state should suspend the normal timer module state resolution,
   * e.g., a roaming Vanu module that should expire will not expire unless the charging process fails.
   * @param module Vanu module to be charged
   * @param beam Vanu module core beam that is involved in the charge process
   * @param player player overseeing the Vanu module charge process;
   *               in general, should be the player who is currently carrying the module
   */
  private def startChargingModule(module: VanuModule, beam: VanuModuleBeam, player: Player): Unit = {
    zone.LocalEvents ! MessageEnvelope(zone.id, PlanetsideAttribute(player.GUID, 73, 1)) //charging pose and progress UI
    //todo ChatMessageType.UNK_227 -> @ChargingVanuModuleMessage
    //it takes 60 second to charge a module; we can not change the rate of the aforementioned progress bar att
    module.Charged = VanuModuleManager.Charging(60, beam, player, module.Charged)
    player.Actor ! Damageable.MakeInvulnerable
  }

  /**
   * The end of the process of transforming an unpowered Vanu module into a charged (powered) Vanu Module.
   * This does not indicate whether the process succeeded or failed,
   * merely that clean-up to the state of all associated participants to the process should occur.
   * @param module Vanu module to be charged
   * @param player player overseeing the Vanu module charge process;
   *               in general, should be the player who is currently carrying the module
   */
  private def stopChargingModule(module: VanuModule, player: Player): Unit = {
    zone.LocalEvents ! MessageEnvelope(zone.id, PlanetsideAttribute(player.GUID, 73, 0)) //undo charging pose
    player.Actor ! Damageable.MakeVulnerable
    stopChargingModuleBasics(module)
  }

  /**
   * The end of the process of transforming an unpowered Vanu module into a charged (powered) Vanu Module.
   * Attempt to restore the previous module state before the charging process started.
   * If this process was started appropriately, the correct previous state will be maintained on the old charge state;
   * otherwise, set the module to being unpowered and
   * let the Vanu module lifecycle sort the appropriate state of the module.
   * This does not indicate whether the process succeeded or failed,
   * merely that clean-up to the state of all associated participants to the process should occur.
   * @param module Vanu module to be charged
   */
  private def stopChargingModuleBasics(module: VanuModule): Unit = {
    val newChargeState = module.Charged match {
      case state: VanuModuleManager.Charging => state.previousChargeState
      case _ => ModuleChargeState.NonPowered
    }
    module.Charged = newChargeState
  }

  /**
   * The end of the process of transforming an unpowered Vanu module into a charged (powered) Vanu Module.
   * This DOES indicate that the process succeeded BUT
   * the Vanu module must still have been involved with the process based on its module charge state.
   * @param module Vanu module to be charged
   * @param beam Vanu module core beam that is involved in the charge process
   * @param player player overseeing the Vanu module charge process;
   *               in general, should be the player who is currently carrying the module
   */
  private def chargeComplete(module: VanuModule, beam: VanuModuleBeam, player: Player): Unit = {
    val chargeState = module.Charged
    stopChargingModule(module, player)
    if (chargeState.isInstanceOf[VanuModuleManager.Charging]) {
      chargeModule(module)
    }
  }

  /**
   * Complete the process of charging a Vanu module.
   * @param module Vanu module to be charged
   */
  private def chargeModule(module: VanuModule): Unit = {
    module.Carrier match {
      case Some(player) =>
        chargeModuleHeldByPlayer(module, player)
      case None =>
        chargeModuleAlone(module)
    }
  }

  /**
   * Complete the process of charging a Vanu module.
   * The module is being carried,
   * so the carrying player must be made fully aware that the now-charged module is to be carried.
   * @param module Vanu module to be charged
   * @param player player overseeing the Vanu module charge process;
   *               in general, should be the player who is currently carrying the module
   */
  private def chargeModuleHeldByPlayer(module: VanuModule, player: Player): Unit = {
    val zoneid = zone.id
    chargeModuleBasics(module)
    val playerPosition = player.Position
    module.Position = Vector3.Zero //spawn far out of sight
    val flagOcmPacket = OCM.apply(module)
    module.Position = playerPosition
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, playerPosition, 15, volume = 0.8f)),
      MessageEnvelope(zoneid, SendResponse(
        flagOcmPacket, /* overwrite with charged module */
        ObjectAttachMessage(player.GUID, module.GUID, Player.SpecialCarrySlot) /* charged module held by player */
      ))
    )
  }

  /**
   * Complete the process of charging a Vanu module.
   * Normally, an uncarried roaming module should never be charged but ...
   * @param module Vanu module to be charged
   */
  private def chargeModuleAlone(module: VanuModule): Unit = {
    val zoneid = zone.id
    chargeModuleBasics(module)
    zone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleStabilized, module.Position, 15, volume = 0.8f)),
      MessageEnvelope(zoneid, SendResponse(OCM.apply(module))) /* overwrite with charged module */
    )
  }

  /**
   * Complete the process of charging a Vanu module.
   * Set the appropriate charge state,
   * clear association with the origin spawner, if necessary,
   * and flag that the module will have it's roaming timer renewed if it should leave the zone in which it spawned.
   * Charging a module can undo an expiration state.
   * @see [[net.psforever.objects.zones.ZoneAware.Continent]]
   * @param module Vanu module to be charged
   */
  private def chargeModuleBasics(module: VanuModule): Unit = {
    module.Charged = ModuleChargeState.Charged
    module.Continent = "spawnCavern" //timer renewal flag
    VanuModuleNode.ThatSpawnedThis(module).ClearModule()
  }

  /**
   * Manipulate a Vanu module that is being held (common) and place it into a receptacle for a Vanu module.
   * The module will no longer count as roaming and the timer for that is cleared.
   * If this is the first time the module is being installed in a receptacle,
   * a one-time installation countdown will be set
   * and will persist until changes at the end of the module's lifecycle.
   * The faction who owns the receptacle gains system-wide acknowledgement that it owns the module.
   * This counts as a "risky action".
   * @param module Vanu module to be installed into the Vanu module receptacle
   * @param cradle receptacle for the Vanu module
   * @param player player overseeing the Vanu module installation;
   *               in general, should be the player who is currently carrying the module
   */
  private def installModuleIntoCradle(module: VanuModule, cradle: VanuModuleNode, player: Player): Unit = {
    cradle.captureFlag match {
      case None
        if succeedAtRiskyInstallation(module) && TryGiveFactionModuleOwnership(cradle.Faction, module.FlagFaction) =>
        val moduleGuid = module.GUID
        val zone = cradle.Zone
        val owner = cradle.Owner.asInstanceOf[Building]
        SetModuleInstallationTime(module)
        module.SetRoamingTime(Long.MaxValue) //deactivate
        module.Charged = ModuleChargeState.Charged
        module.Position = cradle.Position + FlagHoverHeight
        module.Orientation = cradle.Orientation
        module.Target = owner
        module.Faction = cradle.Faction
        module.Carrier = None
        module.WhichSide = cradle.WhichSide
        player.Carrying = None
        cradle.captureFlag = module
        cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(cradle.GUID, moduleGuid, 0))),
          MessageEnvelope(player.Name, LocalAction.PlayerDropsFlag(moduleGuid))
        )

      case _ => ()
    }
  }

  /**
   * The Vanu module installed in the receptacle will break free from that receptacle and appear to hover above it.
   * No player is available is assume the role of carrier.
   * If the module receptacle is not a spawning cradle,
   * the module will no longer be considered owned by the faction that owns the receptacle.
   * @param cradle receptacle where the Vanu module was installed
   */
  private def freeModuleFromCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(module: VanuModule)
        if canHaveMoreRoamingModulesInZone =>
        val zone = cradle.Zone
        val zoneid = zone.id
        val flagPosition = cradle.Position + FlagHoverHeight
        val flagOrientation = cradle.Orientation
        module.SetRoamingTime(GetIdealRoamingTime(module, suggestedTime = 900000L)) //(ms; or, 15 minutes)
        if (cradle.ValidFlagType != module.ValidFlagType) {
          RemoveModuleFromFactionOwnership(cradle.Faction)
        }
        cradle.captureFlag = None
        module.Target = Building.NoBuilding
        module.Position = flagPosition
        module.Orientation = flagOrientation
        zone.LocalEvents ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleDrop, flagPosition, 15, volume = 0.8f)),
          MessageEnvelope(zoneid, SendResponse(ObjectDetachMessage(cradle.GUID, module.GUID, flagPosition, 0, 0, flagOrientation.z)))
        )
        cradle.Owner.Actor ! BuildingActor.AmenityStateChange(cradle)
      case _ => ()
    }
  }

  /**
   * When a cavern module facility's hack is complete,
   * and the facility has changed faction ownership,
   * all modules that were spawned by that facility that are still physically present in the cavern will be disposed of.
   * Roaming modules will explode immediately.
   * Installed modules, i.e., uncollected unpowered modules, will be deconstructed quietly.
   * @param facility cavern facility
   */
  private def moduleFacilityLost(facility: Building): Unit = {
    val (compromisedModules, retainedModules) = trackedModules.partition(_.Owner eq facility)
    val (installedModules, roamingModules) = compromisedModules.partition(_.isInstalled)
    trackedModules = retainedModules
    roamingModules.foreach { module =>
      explodeModule(module)
      startModuleCleanup(module)
    }
    installedModules.foreach(startModuleCleanup)
  }

  /* vanu module spawn support functions */

  /**
   * Based off of a Vanu module spawning cradle,
   * construct a new Vanu module that will be instaleld into that cradle.
   * @param spawner target Vanu module spawning cradle
   */
  private def trySpawnModuleInCradle(spawner: VanuModuleNode): Unit = {
    spawner.captureFlag match {
      case None if CradleIsFunctionalStatus(spawner) =>
        val zone = spawner.Zone
        val module = new VanuModule(GlobalDefinitions.vanu_module, spawner.ValidFlagType)
        val position = spawner.Position + FlagHoverHeight
        val orientation = spawner.Orientation
        module.Zone = zone
        module.Position = position
        module.Orientation = orientation
        module.WhichSide = spawner.WhichSide
        module.Charged = VanuModuleManager.Demo
        TaskWorkflow.execute(VanuModuleRegistrationBeforeTask(zone, module, onSpawnBehaviors(spawner)))
      case _ => ()
    }
  }

  /**
   * Appropriately deal with a newly-spawned and registered Vanu module,
   * completing the cradle installation process.
   * @param cradle target Vanu module spawning cradle, curried
   * @param thisZone zone to which the Vanu module was registered
   * @param module newly-spawned and registered Vanu module
   */
  private def onSpawnBehaviors(
                                cradle: VanuModuleNode
                              )
                              (
                                thisZone: Zone,
                                module: VanuModule
                              ): Unit = {
    val zone = cradle.Zone
    val zoneid = thisZone.id
    val owner = cradle.Owner.asInstanceOf[Building]
    module.Target = owner
    module.Faction = cradle.Faction
    cradle.captureFlag = module
    trackModule(module)
    thisZone.LocalEvents ! BundledEnvelope(
      MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.LLUMaterialize, module.Position, 15, volume = 0.8f)),
      MessageEnvelope(zoneid, GenericObjectAction(
        cradle.GUID,
        GenericObjectActionEnum.FlagSpawned.id
      )),
      MessageEnvelope(zoneid, SendResponse(OCM.apply(module)))
    )
    thisZone.LocalEvents ! MessageEnvelope(zone.id, SendResponse(ObjectAttachMessage(cradle.GUID, module.GUID, 0)))
    owner.Actor ! BuildingActor.AmenityStateChange(cradle)
    //todo broadcast chat message for module spawn?
  }

  /**
   * When a Vanu module is transported between zones, it must be registered to the new zone for proper introduction.
   * If the module does not enter the zone,
   * it will typically not be registered to any zone, appear spawned in any zone, and not be tracked by any zone
   * so it can be easily eliminated by ignoring its existence.
   * The downstream of this counts as a "risky action".
   * @param module roaming Vanu module that is considered "between zones"
   * @param player player who is responsible for accepting the Vanu module in the new zone (becomes its carrier)
   */
  private def newModuleInZone(module: VanuModule, player: Player): Unit = {
    if ((module.Charged.value || (module.Owner.Zone eq zone)) && canHaveMoreRoamingModulesInZone) {
      TaskWorkflow.execute(VanuModuleRegistrationBeforeTask(zone, module, onEntryBehavior))
    } else if (module.Carrier.contains(player)) {
      //module does not enter zone
      module.Carrier = None
      player.Carrying = None
      VanuModuleNode.ThatSpawnedThis(module).ClearModule()
    }
  }

  /**
   * Appropriately deal with a newly-spawned and registered Vanu module,
   * making certain it is acknowledged by the new zone and by any player who carried it between zones.
   * If this is the first time the module has been removed from its spawning zone, renew the roaming timer.
   * This counts as a "risky action" if the module is being carried.
   * @param thisZone zone to which the Vanu module was registered
   * @param module newly-spawned and registered Vanu module
   */
  private def onEntryBehavior(
                               thisZone: Zone,
                               module: VanuModule
                             ): Unit = {
    val zoneid = thisZone.id
    val events = thisZone.LocalEvents
    val flagGuid = module.GUID
    lazy val flagCreationPacketMsg = MessageEnvelope(zoneid, SendResponse(OCM.apply(module)))
    if (module.Continent.equals("spawnCavern")) {
      module.SetRoamingTime(durationTime = 900000L) //ms; or, 15 minutes
    }
    module.Zone = zone
    trackModule(module)
    module.Carrier
      .map { carrier =>
        if (succeedAtRiskyAction(module)) {
          val flagCategory = module.ValidFlagType.category
          val flagNewPosition = carrier.Position + FlagHoverHeight
          module.Carrier = Some(carrier)
          carrier.Carrying = Some(flagCategory)
          module.Position = flagNewPosition
          events ! BundledEnvelope(
            flagCreationPacketMsg,
            MessageEnvelope(zoneid, SendResponse(ObjectAttachMessage(carrier.GUID, flagGuid, Player.SpecialCarrySlot))),
            MessageEnvelope(carrier.Name, LocalAction.PlayerCollectsFlag(flagCategory, flagGuid))
          )
          Some(carrier)
        } else {
          None
        }
      }
      .getOrElse {
        events ! BundledEnvelope(
          MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.LLUMaterialize, module.Position, 15, volume = 0.8f)),
          flagCreationPacketMsg
        )
      }
  }

  /* vanu module deconstruct support functions */

  /**
   * A general starting point for all Vanu module deconstruct activity.
   * If the module is installed in a cradle, disassociate it from the cradle before proceeding.
   * Otherwise, go to check if the module is being carried.
   * @param module Vanu module being deconstructed
   */
  private def startModuleCleanup(module: VanuModule): Unit = {
    VanuModuleNode
      .ThatContainsThis(module)
      .GetSpawner
      .map { cradle =>
        tryDespawnModuleAssociatedWithCradle(cradle, module)
        cradle
      }
      .getOrElse(tryDespawnModuleIfCarried(module))
  }

  /**
   * A general starting point for Vanu module deconstruct activity related to receptacle installation.
   * The module to be deconstructed must be the same module installed in the cradle to proceed.
   * @param cradle Vanu module receptacle
   * @param module Vanu module being deconstructed
   */
  private def tryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode, module: VanuModule): Unit = {
    cradle.captureFlag match {
      case Some(flag) if flag eq module =>
        tryDespawnModuleAssociatedWithCradle(cradle)
      case _ =>
        tryDespawnModuleIfCarried(module)
    }
  }

  /**
   * A general starting point for Vanu module deconstruct activity related to receptacle installation.
   * If the receptacle is a spawner, do one thing.
   * If the receptacle is for properly installation and integration of benefits, do something else.
   * @param cradle Vanu module receptacle
   */
  private def tryDespawnModuleAssociatedWithCradle(cradle: VanuModuleNode): Unit = {
    cradle.captureFlag match {
      case Some(flag: VanuModule) if cradle.ValidFlagType == CarriableFlag.VanuModuleCradle =>
        tryDespawnModuleInstalledInCradle(cradle, flag)
      case Some(flag: VanuModule) =>
        tryDespawnModuleInstalledInNode(cradle, flag)
      case _ =>
        ()
    }
  }

  /**
   * A specific part of the Vanu module deconstruction activity related to receptacle installation.
   * The faction that possessed this cradle no longer possesses ownership of the module (that was installed).
   * @param cradle Vanu module receptacle
   * @param module Vanu module being deconstructed
   */
  private def tryDespawnModuleInstalledInCradle(cradle: VanuModuleNode, module: VanuModule): Unit = {
    RemoveModuleFromFactionOwnership(cradle.Faction)
    tryDespawnModuleInstalledInNode(cradle, module)
  }

  /**
   * A specific part of the Vanu module deconstruction activity related to receptacle installation.
   * @param cradle Vanu module receptacle
   * @param module Vanu module being deconstructed
   */
  private def tryDespawnModuleInstalledInNode(cradle: VanuModuleNode, module: VanuModule): Unit = {
    val cradleOwner = cradle.Owner
    cradle.captureFlag = None
    cradleOwner.Actor ! BuildingActor.AmenityStateChange(cradle)
    tryDespawnModule(module)
  }

  /**
   * A specific part of the Vanu module deconstruction activity related to being carried by a player.
   * Since the ultimate goal is deconstruction of the module,
   * not being carried is not an issue,
   * and the specific changes to the module can be skipped if it really isn't being carried.
   * @param module Vanu module being deconstructed
   */
  private def tryDespawnModuleIfCarried(module: VanuModule): Unit = {
    module.Carrier match {
      case Some(carrier) =>
        module.Carrier = None
        carrier.Carrying = None
        module.Position = carrier.Position + FlagHoverHeight
        carrier.Zone.AvatarEvents ! MessageEnvelope(carrier.Name, LocalAction.PlayerDropsFlag(module.GUID))
      case None => ()
    }
    tryDespawnModule(module)
  }

  /**
   * A specific part of the Vanu module deconstruction activity;
   * the final part of the Vanu module deconstruction activity.
   * No reason left to instaklled, carry, track, time, render, or keep registered the module.
   * Just let it go.
   * @param module Vanu module being deconstructed
   */
  private def tryDespawnModule(module: VanuModule): Unit = {
    val zone = module.Zone
    val zoneid = zone.id
    untrackModule(module)
    module.SetRoamingTime(Long.MaxValue)
    module.SetExpirationTime(Long.MaxValue)
    if (module.Destroyed) {
      zone.LocalEvents ! MessageEnvelope(zoneid, ObjectDelete(module.GUID))
    } else {
      zone.LocalEvents ! BundledEnvelope(
        MessageEnvelope(zoneid, LocalAction.TriggerSound(TriggeredSound.ModuleExpiration, module.Position, unk=20, volume=0.8000001f)),
        MessageEnvelope(zoneid, ObjectDelete(module.GUID))
      )
    }
    TaskWorkflow.execute(GUIDTask.unregisterObject(zone.GUID, module))
  }
}
