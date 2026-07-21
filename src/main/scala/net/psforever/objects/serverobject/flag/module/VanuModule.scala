// Copyright (c) 2026 PSForever
package net.psforever.objects.serverobject.flag.module

import net.psforever.objects.serverobject.flag.base.{FlagDefinition, FlagType, OwnedFlag}

class VanuModule(fDef: FlagDefinition, ValidFlagType: FlagType)
  extends OwnedFlag(fDef, ValidFlagType) {
  private var charged: Boolean = false

  def Charged: Boolean = charged

  def Charged_=(chargeState: Boolean): Boolean = {
    charged = chargeState
    Charged
  }
}
