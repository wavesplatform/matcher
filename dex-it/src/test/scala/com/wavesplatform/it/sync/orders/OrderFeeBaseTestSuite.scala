package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.it.MatcherSuiteBase

class OrderFeeBaseTestSuite extends MatcherSuiteBase {

  val percentFee = 14

  val price                = 1.2.usd
  val fullyAmountWaves     = 15.waves
  val partiallyAmountWaves = 9.waves
  val fullyAmountUsd       = 18.usd
  val minimalFee           = 4.5.usd
  val partiallyFeeUsd      = 2.7.usd
  val partiallyAmountUsd   = 10.8.usd
  val tooLowFee            = 2.51.usd
  val tooHighFee           = 18.01.usd
  val minimalFeeWaves      = 3.75.waves
  val tooLowFeeWaves       = 2.09.waves
  val tooHighFeeWaves      = 15.00001.waves
  val partiallyFeeWaves    = 2.25.waves

  def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        assert(
          wavesNode1.api.balance(alice, asset) >= balance,
          s"Bob doesn't have enough balance in ${asset.toString} to make a transfer"
        )
        broadcastAndAwait(mkTransfer(alice, account.toAddress, balance, asset))
      }
    }
    account
  }
}
