package com.wavesplatform.dex

import io.qameta.allure.Story
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

@Story("waves-ext")
trait WavesExtSuiteBase extends AnyFreeSpecLike with Matchers with AllureScalatestContext
