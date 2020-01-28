package com.wavesplatform.dex.caches

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.settings.OrderFeeSettings.{DynamicSettings, OrderFeeSettings, PercentSettings}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class OrderFeeSettingsCacheSpecification extends AnyWordSpecLike with Matchers with MatcherSpecBase {

  "OrderFeeSettingsCache" should {

    "prevent matcher start if zero offset settings wasn't set" in {
      a[IllegalArgumentException] should be thrownBy new OrderFeeSettingsCache(Map(2L -> DynamicSettings(0.001.waves, 0.005.waves)), 0L)
    }

    "correctly retrieve current fee settings" in {

      val settingsMap =
        Map(
          0L    -> DynamicSettings.symmetric(0.003.waves),
          2L    -> DynamicSettings(0.001.waves, 0.005.waves),
          1000L -> PercentSettings(AssetType.AMOUNT, 0.005)
        )

      var offset = 0L
      val ofsc   = new OrderFeeSettingsCache(settingsMap, offset)

      def check(newOffsetValue: Long, current: OrderFeeSettings, actual: OrderFeeSettings): Unit = {
        offset = newOffsetValue
        ofsc.getCurrentFeeSettings should matchTo(current)
        ofsc.getFeeSettingsForNextOrder should matchTo(actual)
      }

      check(newOffsetValue = 0L, current = DynamicSettings(0.003.waves, 0.003.waves), actual = DynamicSettings(0.003.waves, 0.003.waves))
      check(newOffsetValue = 1L, current = DynamicSettings(0.003.waves, 0.003.waves), actual = DynamicSettings(0.001.waves, 0.005.waves))
      check(newOffsetValue = 100L, current = DynamicSettings(0.001.waves, 0.005.waves), actual = DynamicSettings(0.001.waves, 0.005.waves))
      check(newOffsetValue = 999L, current = DynamicSettings(0.001.waves, 0.005.waves), actual = PercentSettings(AssetType.AMOUNT, 0.005))
    }
  }
}
