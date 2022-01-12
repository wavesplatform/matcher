package com.wavesplatform.dex.domain

package object model {

  type Price = Long
  type Amount = Long

  /** Converts amounts, prices and fees from denormalized values (decimal numbers) to normalized ones (longs) */
  object Normalization {

    def normalizeAmountAndFee(value: BigDecimal, assetDecimals: Int): Amount =
      (value * BigDecimal(10).pow(assetDecimals)).toLong

    def normalizePrice(value: BigDecimal, amountAssetDecimals: Int, priceAssetDecimals: Int): Price =
      (value * BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals)).toLong

  }

  /** Converts amounts, prices and fees from normalized values (longs) to denormalized ones (decimal numbers) */
  object Denormalization {

    def denormalizeAmountAndFee(value: Amount, assetDecimals: Int): BigDecimal =
      BigDecimal(value) / BigDecimal(10).pow(assetDecimals)

    def denormalizePrice(value: Price, amountAssetDecimals: Int, priceAssetDecimals: Int): BigDecimal =
      BigDecimal(value) / BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals)

    def denormalizeWavesAmount(value: Amount): BigDecimal = denormalizeAmountAndFee(value, 8)
  }

}
