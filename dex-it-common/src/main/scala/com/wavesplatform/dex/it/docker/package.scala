package com.wavesplatform.dex.it

import org.testcontainers.containers.wait.strategy.AbstractWaitStrategy

package object docker {
  val apiKey                                   = "integration-test-rest-api"
  val ignoreWaitStrategy: AbstractWaitStrategy = () => ()
}
