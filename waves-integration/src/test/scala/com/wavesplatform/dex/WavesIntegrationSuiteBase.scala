package com.wavesplatform.dex

import io.qameta.allure.{Feature, Story}
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

@Story("waves-integration")
@Feature("tests")
trait WavesIntegrationSuiteBase extends AnyWordSpecLike with Matchers with AllureScalatestContext
