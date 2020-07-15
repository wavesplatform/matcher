package com.wavesplatform.dex.waves

import com.wavesplatform.dex.asset.DoubleOps.NumericOps

object WavesFeeConstants extends WavesFeeConstants

trait WavesFeeConstants {
  val smartFee: Long                   = 0.004.waves
  val minFee: Long                     = 0.001.waves
  val leasingFee: Long                 = 0.002.waves
  val issueFee: Long                   = 1.waves
  val smartIssueFee: Long              = 1.waves + smartFee
  val burnFee: Long                    = 1.waves
  val sponsorFee: Long                 = 1.waves
  val setAssetScriptFee: Long          = 1.waves
  val setScriptFee: Long               = 0.01.waves
  val transferAmount: Long             = 10.waves
  val leasingAmount: Long              = transferAmount
  val issueAmount: Long                = transferAmount
  val massTransferFeePerTransfer: Long = 0.0005.waves
  val massTransferDefaultFee: Long     = 1.waves
  val someAssetAmount: Long            = 9999999999999L
  val matcherFee: Long                 = 0.003.waves
  val orderFee: Long                   = matcherFee
  val smartMatcherFee: Long            = 0.007.waves
  val smartMinFee: Long                = minFee + smartFee

  val tradeFee: Long         = 0.003.waves
  val smartTradeFee: Long    = tradeFee + smartFee
  val twoSmartTradeFee: Long = tradeFee + 2 * smartFee
}
