package com.wavesplatform.dex

import com.wavesplatform.dex.domain.utils.ScorexLogging

package object app extends ScorexLogging {

  def forceStopApplication(reason: ApplicationStopReason): Unit = {
    log.error(s"The force stop was called: ${reason.getMessage}")
    System.exit(reason.code)
  }

}
