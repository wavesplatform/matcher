package com.wavesplatform.dex.caches

import com.wavesplatform.dex.caches.OrderFeeSettingsCache.{AssetsActionForOffset, CustomAssetFeeState}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CustomAssetFeeStateSpec extends AnyWordSpecLike with Matchers {

  private val asset1: Asset = Asset.IssuedAsset(ByteStr.decodeBase58("DWgwcZTMhSvnyYCoWLRUXXSH1RSkzThXLJhww9gwkqdn").get)
  private val asset2: Asset = Asset.IssuedAsset(ByteStr.decodeBase58("2GBgdhqMjUPqreqPziXvZFSmDiQVrxNuGxR1z7ZVsm4Z").get)
  private val asset3: Asset = Asset.IssuedAsset(ByteStr.decodeBase58("Euz5HtYcj3nVTZxppA7wdabwTe5BzHFiu4QG1EJtzeUx").get)
  private val asset4: Asset = Asset.IssuedAsset(ByteStr.decodeBase58("5UYBPpq4WoU5n4MwpFkgJnW3Fq4B1u3ukpK33ik4QerR").get)
  private val asset5: Asset = Asset.IssuedAsset(ByteStr.decodeBase58("8zUYbdB8Q6mDhpcXYv52ji8ycfj4SDX4gJXS7YY3dA4R").get)

  private val defaultAssetMap = Map(
    5L -> Set(asset1),
    10L -> Set(asset1, asset2),
    11L -> Set(asset1, asset2, asset3, asset4),
    15L -> Set(asset2, asset3, asset4),
    16L -> Set(asset2, asset3, asset4, asset5),
    18L -> Set(asset2, asset4, asset5)
  )

  "CustomAssetFeeState" should {

    "return empty Set[Asset] if there is no actions" in {
      val state = CustomAssetFeeState.empty
      state.getForLatestOffset() shouldBe empty
      state.latestAssetOffsetOpt shouldBe empty
      (0 to 10).map { i =>
        state.getAssetsForOffset(i + (i * 5)) shouldBe empty
      }
    }

    "return proper state for offset" in {
      val state = CustomAssetFeeState(defaultAssetMap)
      checkAccordingToMap(state)
    }

    "apply AssetsActionForOffset" in {
      val actions = Seq(
        AssetsActionForOffset(5L, Set(asset1), isAdded = true),
        AssetsActionForOffset(10L, Set(asset2), isAdded = true),
        AssetsActionForOffset(11L, Set(asset3, asset4), isAdded = true),
        AssetsActionForOffset(15L, Set(asset1), isAdded = false),
        AssetsActionForOffset(16L, Set(asset5), isAdded = true),
        AssetsActionForOffset(18L, Set(asset3), isAdded = false)
      )
      val state = actions.foldLeft(CustomAssetFeeState.empty) {
        case (acc, elem) => acc.applyAssetsActionForOffset(elem)
      }
      checkAccordingToMap(state)
    }

  }

  private def checkAccordingToMap(state: CustomAssetFeeState) = {
    state.getForLatestOffset() shouldBe Set(asset2, asset4, asset5)
    state.latestAssetOffsetOpt shouldBe Some(18L)

    state.getAssetsForOffset(0) shouldBe empty
    state.getAssetsForOffset(5) shouldBe empty
    state.getAssetsForOffset(6) shouldBe Set(asset1)
    state.getAssetsForOffset(10) shouldBe Set(asset1)
    state.getAssetsForOffset(11) shouldBe Set(asset1, asset2)
    state.getAssetsForOffset(12) shouldBe Set(asset1, asset2, asset3, asset4)
    state.getAssetsForOffset(15) shouldBe Set(asset1, asset2, asset3, asset4)
    state.getAssetsForOffset(16) shouldBe Set(asset2, asset3, asset4)
    state.getAssetsForOffset(17) shouldBe Set(asset2, asset3, asset4, asset5)
    state.getAssetsForOffset(19) shouldBe Set(asset2, asset4, asset5)
    state.getAssetsForOffset(20) shouldBe Set(asset2, asset4, asset5)
    state.getAssetsForOffset(25) shouldBe Set(asset2, asset4, asset5)
  }

}
