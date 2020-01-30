package com.wavesplatform.dex.caches

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, OrderFeeSettings, PercentSettings}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class OrderFeeSettingsCacheSpecification extends AnyWordSpecLike with Matchers with MatcherSpecBase {

  "OrderFeeSettingsCache" should {

    "prevent matcher start if settings wasn't set" in {
      a[IllegalArgumentException] should be thrownBy new OrderFeeSettingsCache(Map.empty[Long, OrderFeeSettings])
    }

    "throw error if there is no settings for the given offset" in {
      a[IllegalStateException] should be thrownBy new OrderFeeSettingsCache(Map(100L -> DynamicSettings.symmetric(0.003.waves)))
        .getSettingsForOffset(1)
    }

    "correctly retrieve current fee settings" in {

      val settingsMap =
        Map(
          -1L   -> DynamicSettings.symmetric(0.003.waves),
          2L    -> DynamicSettings(0.001.waves, 0.005.waves),
          15L   -> DynamicSettings(0.002.waves, 0.004.waves),
          1000L -> PercentSettings(AssetType.AMOUNT, 0.005)
        )

      val ofsc = new OrderFeeSettingsCache(settingsMap)

      def check(offset: Long, current: OrderFeeSettings, actual: OrderFeeSettings): Unit = {
        ofsc.getSettingsForOffset(offset) should matchTo(current)
        ofsc.getSettingsForOffset(offset + 1) should matchTo(actual)
      }

      check(offset = -1L, current = DynamicSettings(0.003.waves, 0.003.waves), actual = DynamicSettings(0.003.waves, 0.003.waves))
      check(offset = 0L, current = DynamicSettings(0.003.waves, 0.003.waves), actual = DynamicSettings(0.003.waves, 0.003.waves))
      check(offset = 17L, current = DynamicSettings(0.002.waves, 0.004.waves), actual = DynamicSettings(0.002.waves, 0.004.waves))
      check(offset = 100L, current = DynamicSettings(0.002.waves, 0.004.waves), actual = DynamicSettings(0.002.waves, 0.004.waves))
      check(offset = 999L, current = DynamicSettings(0.002.waves, 0.004.waves), actual = PercentSettings(AssetType.AMOUNT, 0.005))
    }
  }
}
