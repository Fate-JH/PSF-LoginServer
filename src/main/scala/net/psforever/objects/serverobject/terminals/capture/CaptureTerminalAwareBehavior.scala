package net.psforever.objects.serverobject.terminals.capture

import akka.actor.Actor.Receive
import net.psforever.objects.serverobject.structures.Amenity
import net.psforever.types.PlanetSideEmpire

import scala.annotation.unused

/**
  * The behaviours corresponding to an Amenity that is marked as being CaptureTerminalAware
  * @see CaptureTerminalAware
  */
trait CaptureTerminalAwareBehavior {
  def CaptureTerminalAwareObject: Amenity with CaptureTerminalAware

  val captureTerminalAwareBehaviour: Receive = {
    case CaptureTerminalAwareBehavior.TerminalStatusChanged(terminal, TerminalStatusAction.Captured(_, _)) =>
      captureTerminalIsCaptured(terminal)

    case CaptureTerminalAwareBehavior.TerminalStatusChanged(terminal, TerminalStatusAction.Resecured) =>
      captureTerminalIsResecured(terminal)

    case CaptureTerminalAwareBehavior.TerminalStatusChanged(terminal, TerminalStatusAction.Hacked(_)) =>
      captureTerminalIsHacked(terminal)
  }

  protected def captureTerminalIsCaptured(@unused terminal: CaptureTerminal): Unit = captureTerminalIsResecured(terminal)

  protected def captureTerminalIsResecured(@unused terminal: CaptureTerminal): Unit = { /* intentionally blank */ }

  protected def captureTerminalIsHacked(@unused terminal: CaptureTerminal): Unit = { /* intentionally blank */ }
}

trait TerminalStatusAction

object TerminalStatusAction {
  final case class Hacked(byFaction: PlanetSideEmpire.Value) extends TerminalStatusAction

  final case object Resecured extends TerminalStatusAction

  final case class Captured(fromFaction: PlanetSideEmpire.Value, toFaction: PlanetSideEmpire.Value) extends TerminalStatusAction
}

object CaptureTerminalAwareBehavior {
  final case class TerminalStatusChanged(terminal: CaptureTerminal, info: TerminalStatusAction)

  def ComposeStatusChangeMessage(
                                  terminal: CaptureTerminal,
                                  isResecured: Boolean,
                                  fromFaction: PlanetSideEmpire.Value,
                                  toFaction: PlanetSideEmpire.Value
                                ): CaptureTerminalAwareBehavior.TerminalStatusChanged = {
    val info: TerminalStatusAction = if (isResecured) {
      if (fromFaction == toFaction) {
        TerminalStatusAction.Resecured
      } else {
        TerminalStatusAction.Captured(fromFaction, toFaction)
      }
    } else {
      TerminalStatusAction.Hacked(toFaction)
    }
    CaptureTerminalAwareBehavior.TerminalStatusChanged(terminal, info)
  }
}
