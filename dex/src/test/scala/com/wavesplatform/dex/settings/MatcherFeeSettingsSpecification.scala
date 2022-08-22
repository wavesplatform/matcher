package com.wavesplatform.dex.settings

import cats.syntax.option._
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.settings.OrderFeeSettings.{CompositeSettings, DynamicSettings, PercentSettings}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

final class MatcherFeeSettingsSpecification extends BaseSettingsSpecification with Matchers with DiffMatcherWithImplicits with OptionValues {

  private val asset1 = AssetPair.extractAsset("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get
  private val asset2 = AssetPair.extractAsset("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J").get
  private val asset3 = AssetPair.extractAsset("AbunLGErT5ctzVN8MVjb4Ad9YgjpubB8Hqb17VxzfAck").get

  private val assetPair1Waves = AssetPair(asset1, Waves)
  private val assetPair2Waves = AssetPair(asset2, Waves)
  private val assetPair12 = AssetPair(asset1, asset2)
  private val assetPair3Waves = AssetPair(asset3, Waves)

  val feeStringConfig =
    s"""
       |order-fee {
       |  -1: {
       |    mode = percent
       |    percent {
       |      asset-type = amount
       |      min-fee = 0.1
       |      min-fee-in-waves = 300000
       |    }
       |  }
       |
       |  5: {
       |    mode = composite
       |    composite {
       |        custom {
       |          $asset1-WAVES {
       |            mode = "percent"
       |            percent {
       |              asset-type = "amount"
       |              min-fee = 0.01
       |              min-fee-in-waves = 1000
       |            }
       |          }
       |          $asset2-WAVES {
       |            mode = "percent"
       |            percent {
       |              asset-type = "amount"
       |              min-fee = 0.01
       |              min-fee-in-waves = 1000
       |            }
       |          }
       |
       |        }
       |
       |        default {
       |          mode = "dynamic"
       |          dynamic {
       |            base-maker-fee = 350000
       |            base-taker-fee = 350000
       |          }
       |        }
       |
       |        custom-assets {
       |            # WAVES or Issued asset ID in base 58
       |            assets = ["$asset2", "WAVES", "$asset1"]
       |
       |            # will be applied to all pairs made from matching assets from config above
       |            settings {
       |                mode = "percent"
       |                percent {
       |                  asset-type = "spending"
       |                  min-fee = 0.1
       |                  min-fee-in-waves = 300000
       |                }
       |            }
       |        }
       |      }
       |  }
       |}
       """.stripMargin

  private val invalidAssetType =
    s"""
       |order-fee {
       |  -1: {
       |    mode = percent
       |    dynamic {
       |      base-maker-fee = 300000
       |      base-taker-fee = 300000
       |    }
       |    fixed {
       |      asset = WAVES
       |      min-fee = 300000
       |    }
       |    percent {
       |      asset-type = test
       |      min-fee = 121.2
       |    }
       |  }
       |}
       """.stripMargin

  private val invalidPercent =
    s"""
       |order-fee {
       |  -1: {
       |    mode = percent
       |    dynamic {
       |      base-maker-fee = 300000
       |      base-taker-fee = 300000
       |    }
       |    fixed {
       |      asset = WAVES
       |      min-fee = 300000
       |    }
       |    percent {
       |      asset-type = spending
       |      min-fee = 121.2
       |    }
       |  }
       |}
       """.stripMargin

  private val invalidAsset =
    s"""
       |order-fee {
       |  -1: {
       |    mode = fixed
       |    dynamic {
       |      base-maker-fee = 300000
       |      base-taker-fee = 300000
       |    }
       |    fixed {
       |      asset = ;;;;
       |      min-fee = -300000
       |    }
       |    percent {
       |      asset-type = test
       |      min-fee = 50
       |    }
       |  }
       |}
       """.stripMargin

  private val invalidFee =
    s"""
       |order-fee {
       |  -1: {
       |    mode = fixed
       |    dynamic {
       |      base-maker-fee = 300000
       |      base-taker-fee = 300000
       |    }
       |    fixed {
       |      asset = DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
       |      min-fee = -300000
       |    }
       |    percent {
       |      asset-type = test
       |      min-fee = 121
       |    }
       |  }
       |}
       """.stripMargin

  private val invalidFeeInDynamicMode =
    s"""
       |order-fee {
       |  -1: {
       |    mode = dynamic
       |    dynamic {
       |      base-maker-fee = -350000
       |      base-taker-fee = 350000
       |    }
       |    fixed {
       |      asset = ;;;;
       |      min-fee = -300000
       |    }
       |    percent {
       |      asset-type = test
       |      min-fee = 121
       |    }
       |  }
       |}
       """.stripMargin

  "OrderFeeSettings" should "read values" in {
    val validPairs = Set(assetPair1Waves, assetPair2Waves, assetPair3Waves, assetPair12)

    val config = configWithSettings(orderFeeStr = feeStringConfig)
    val orderFeeSettings =
      ConfigSource.fromConfig(config).at("waves.dex").loadOrThrow[MatcherSettings].orderFee.map(v => (v._1, v._2(validPairs.contains)))
    orderFeeSettings.get(-1).value shouldBe PercentSettings(AssetType.Amount, minFee = 0.1, minFeeInWaves = 300000)

    val compositeSettings = orderFeeSettings.get(5).value.asInstanceOf[CompositeSettings]
    compositeSettings.customAssets.value.settingsMap.keySet shouldBe CompositeSettings.CustomAssetsSettings(
      assets = Set(asset2, Waves, asset1),
      settings = PercentSettings(AssetType.Spending, minFee = 0.1, minFeeInWaves = 300000),
      validPairs.contains
    ).settingsMap.keySet

    compositeSettings.default shouldBe DynamicSettings(baseMakerFee = 350000, baseTakerFee = 350000)
    compositeSettings.custom shouldBe Map(
      assetPair1Waves -> PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000),
      assetPair2Waves -> PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
    )

    compositeSettings.customAssets.value.settingsMap shouldBe CompositeSettings.CustomAssetsSettings(
      assets = Set(asset2, Waves, asset1),
      settings = PercentSettings(AssetType.Spending, minFee = 0.1, minFeeInWaves = 300000),
      validPairs.contains
    ).settingsMap

    // specified for these pairs
    compositeSettings.getOrderFeeSettings(assetPair1Waves) shouldBe PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
    compositeSettings.getOrderFeeSettings(assetPair2Waves) shouldBe PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
    // must take default settings
    compositeSettings.getOrderFeeSettings(assetPair3Waves) shouldBe DynamicSettings(baseMakerFee = 350000, baseTakerFee = 350000)
    //must take from custom assets
    compositeSettings.getOrderFeeSettings(assetPair12) shouldBe PercentSettings(AssetType.Spending, minFee = 0.1, minFeeInWaves = 300000)

    getSettingByConfig(configStr(invalidMode())) should produce("waves.dex.order-fee.-1.mode.+\n.+invalid".r)
    getSettingByConfig(configStr(invalidMode("waves"))) should produce("waves.dex.order-fee.-1.mode.+\n.+waves".r)
    getSettingByConfig(configStr(invalidAssetType)) should produce("waves.dex.order-fee.-1.percent.asset-type.+\n.+test".r)
    getSettingByConfig(configStr(invalidPercent)) should produce("order-fee.-1.percent.min-fee")
    getSettingByConfig(configStr(invalidAsset)) should produce("order-fee.-1.fixed.asset.+\n.+;".r)
    getSettingByConfig(configStr(invalidFee)) should produce("order-fee.-1.fixed.min-fee")
    getSettingByConfig(configStr(invalidFeeInDynamicMode)) should produce("order-fee.-1.dynamic.base-maker-fee")
  }

  private def configStr(x: String): Config = configWithSettings(orderFeeStr = x)

  private def invalidMode(invalidModeName: String = "invalid"): String =
    s"""
       |order-fee {
       |  -1: {
       |    mode = $invalidModeName
       |    dynamic {
       |      base-maker-fee = 300000
       |      base-taker-fee = 300000
       |    }
       |    fixed {
       |      asset = WAVES
       |      min-fee = 300000
       |    }
       |    percent {
       |      asset-type = amount
       |      min-fee = 0.1
       |    }
       |  }
       |}
       """.stripMargin

}
