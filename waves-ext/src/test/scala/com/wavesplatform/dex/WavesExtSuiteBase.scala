package com.wavesplatform.dex

import io.qameta.allure.Feature
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

@Feature("waves-ext")
trait WavesExtSuiteBase extends AnyFreeSpecLike with Matchers with AllureScalatestContext
