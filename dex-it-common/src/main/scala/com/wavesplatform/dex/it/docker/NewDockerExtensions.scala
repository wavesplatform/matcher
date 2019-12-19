package com.wavesplatform.dex.it.docker

import cats.Id
import com.wavesplatform.dex.it.api.HasWaitReady
import com.wavesplatform.dex.it.docker.base.BaseContainer
import com.wavesplatform.utils.ScorexLogging

trait NewDockerExtensions extends ScorexLogging {

  protected def startAndWait(container: BaseContainer, api: => HasWaitReady[Id]): Unit = {
    container.start()
    api.waitReady
  }

  protected def invalidateCaches(): Unit = {}

  protected def restartContainer(container: BaseContainer, api: => HasWaitReady[Id]): Unit = {
    container.stop()
    invalidateCaches()
    startAndWait(container, api)
  }
}
