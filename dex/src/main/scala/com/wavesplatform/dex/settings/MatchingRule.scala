package com.wavesplatform.dex.settings

import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta

/** Normalized representation of the matching rule */
case class MatchingRule(startOffset: ValidatedCommandWithMeta.Offset, tickSize: Long) {

  def denormalize(amountAssetDecimals: Int, priceAssetDecimals: Int): DenormalizedMatchingRule =
    DenormalizedMatchingRule(
      startOffset,
      Denormalization.denormalizePrice(tickSize, amountAssetDecimals, priceAssetDecimals)
    )

}

object MatchingRule {
  val DefaultTickSize: Long = 1
  val DefaultRule: MatchingRule = MatchingRule(0L, DefaultTickSize)
}
