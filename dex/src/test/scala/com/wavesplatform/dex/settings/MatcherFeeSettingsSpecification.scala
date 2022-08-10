package com.wavesplatform.dex.settings

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.settings.OrderFeeSettings.{CompositeSettings, DynamicSettings, PercentSettings}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

final class MatcherFeeSettingsSpecification extends BaseSettingsSpecification with Matchers with DiffMatcherWithImplicits with OptionValues {

  "MatcherFeeSettings" should "read values" in {
    val asset1 = AssetPair.extractAsset("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get
    val asset2 = AssetPair.extractAsset("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J").get
    val asset3 = AssetPair.extractAsset("AbunLGErT5ctzVN8MVjb4Ad9YgjpubB8Hqb17VxzfAck").get

    val assetPair1Waves = AssetPair(asset1, Waves)
    val assetPair2Waves = AssetPair(asset2, Waves)
    val assetPair12 = AssetPair(asset1, asset2)
    val assetPair3Waves = AssetPair(asset3, Waves)

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

    val config = configWithSettings(orderFeeStr = feeStringConfig)
    val orderFeeSettings = ConfigSource.fromConfig(config).at("waves.dex").loadOrThrow[MatcherSettings].orderFee

    orderFeeSettings.get(-1).value shouldBe PercentSettings(AssetType.Amount, minFee = 0.1, minFeeInWaves = 300000)
    orderFeeSettings.get(5).value shouldBe CompositeSettings(
      default = DynamicSettings(baseMakerFee = 350000, baseTakerFee = 350000),
      custom = Map(
        assetPair1Waves -> PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000),
        assetPair2Waves -> PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
      ),
      customAssets = CompositeSettings.CustomAssetsSettings(
        assets = Set(asset2, Waves, asset1),
        settings = PercentSettings(AssetType.Spending, minFee = 0.1, minFeeInWaves = 300000)
      ).some
    )
    val compositeSettings = orderFeeSettings.get(5).value.asInstanceOf[CompositeSettings]

    // specified for these pairs
    compositeSettings.getOrderFeeSettings(assetPair1Waves) shouldBe PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
    compositeSettings.getOrderFeeSettings(assetPair2Waves) shouldBe PercentSettings(AssetType.Amount, minFee = 0.01, minFeeInWaves = 1000)
    // must take default settings
    compositeSettings.getOrderFeeSettings(assetPair3Waves) shouldBe DynamicSettings(baseMakerFee = 350000, baseTakerFee = 350000)
    //must take from custom assets
    compositeSettings.getOrderFeeSettings(assetPair12) shouldBe PercentSettings(AssetType.Spending, minFee = 0.1, minFeeInWaves = 300000)

  }

}
