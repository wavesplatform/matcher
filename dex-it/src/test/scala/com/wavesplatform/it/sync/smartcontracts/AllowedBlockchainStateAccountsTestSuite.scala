package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.AccountScriptReturnedError
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase

final class AllowedBlockchainStateAccountsTestSuite extends MatcherSuiteBase {

  "AllowedBlockchainStateAccountsTestSuite" - {

    "should fail while accessing the blockchain state from usual accounts" in {
      updateAccountScript(bob, script1)
      val order = mkOrder(bob, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      dex1.tryApi.place(order) should failWith(
        AccountScriptReturnedError.code,
        "The account's script of 3Q6ujVDbX57oLsXxifqfTcycgb4S8U3DLFz returned the error: An access to the blockchain.height is denied on DEX"
      )
    }

    "should succeed while the blockchain state from usual accounts" in {
      updateAccountScript(carol, script1)
      val carolOrder = mkOrder(carol, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      dex1.api.place(carolOrder)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1, version = 2)
      placeAndAwaitAtDex(aliceOrder, HttpOrderStatus.Status.Filled)
      val txId = ByteStr(dex1.api.getTransactionsByOrderId(carolOrder).head.id().bytes())
      eventually {
        val txInfo = wavesNode1.api.transactionInfo(txId)
        ByteStr(txInfo.id().bytes()) shouldBe txId
      }
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, carol, 1000.waves, Waves))
    dex1.start()
  }

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""
         |waves.dex.order-script-validation.allowed-blockchain-state-accounts=["${carol.publicKey.base58}"]
         |""".stripMargin
    )

  /*
        {-# STDLIB_VERSION 5 #-}
        {-# CONTENT_TYPE EXPRESSION #-}
        {-# SCRIPT_TYPE ACCOUNT #-}
        match tx {
           case eTx: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) && height > 1
           case _ => false
        }
   */
  private lazy val script1 =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAANlVHgFAAAABy" +
      "RtYXRjaDADCQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnByb29mcwAAAAAAAAAAAA" +
      "gFAAAAAnR4AAAAD3NlbmRlclB1YmxpY0tleQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAAEHB2WKqtA="
    )

  private def updateAccountScript(account: KeyPair, script: ByteStr): Unit =
    broadcastAndAwait(mkSetAccountMayBeScript(account, Some(script), fee = setScriptFee + smartFee))

  private lazy val carol = mkKeyPair("carol")

}
