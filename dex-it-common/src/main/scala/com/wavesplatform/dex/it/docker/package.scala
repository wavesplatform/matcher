package com.wavesplatform.dex.it

import org.testcontainers.containers.wait.strategy.AbstractWaitStrategy
import org.testcontainers.utility.TestcontainersConfiguration

package object docker {
  val apiKey = "integration-test-rest-api"
  val ignoreWaitStrategy: AbstractWaitStrategy = () => ()

  TestcontainersConfiguration.getInstance().updateUserConfig("checks.disable", "true")

}
