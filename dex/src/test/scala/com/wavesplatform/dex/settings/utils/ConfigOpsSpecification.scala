package com.wavesplatform.dex.settings.utils


import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ConfigOpsSpecification extends AnyWordSpecLike with Matchers with MatcherSpecBase {


  val config: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  id = "matcher-1"
       |  user = "test-user",
       |  private {
       |    seed = "test-seed",
       |    password = "test-password",
       |    seed58 = "test"
       |  }
       |}""".stripMargin)

  "ConfigOps" should {

    "correctly filter keys" in {
      val filtered = new ConfigOps(config).filterKeys(_.contains("seed"))

      filtered.getString("waves.dex.user") should be ("test-user")
      filtered.getString("waves.dex.private.password") should be ("test-password")
      filtered.getObject("waves.dex.private") should have size 1
    }
  }
}
