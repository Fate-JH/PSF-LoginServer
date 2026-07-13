// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.base

import net.psforever.objects.serverobject.structures.Amenity

trait FlagSocket
  extends Amenity
    with DesignatedFlagType {
  private var lastFlag: Option[OwnedFlag] = None
  private var spawnedCaptureFlag: Option[OwnedFlag] = None

  def captureFlag: Option[OwnedFlag] = spawnedCaptureFlag

  def captureFlag_=(flag: OwnedFlag): Option[OwnedFlag] = captureFlag_=(Some(flag))

  def captureFlag_=(flag: Option[OwnedFlag]): Option[OwnedFlag] = {
    lastFlag = flag.orElse(lastFlag)
    spawnedCaptureFlag = flag
    captureFlag
  }

  def previousFlag: Option[OwnedFlag] = lastFlag

  def clearOldFlagData(): Unit = {
    if (spawnedCaptureFlag.isEmpty) {
      lastFlag = None
    }
  }
}
