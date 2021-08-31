package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.{AccountScriptDeniedOrder, AccountScriptReturnedError}
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.duration._

final class AllowedBlockchainStateAccountsTestSuite extends MatcherSuiteBase {

  "AllowedBlockchainStateAccountsTestSuite" - {

    val allowedAcc = carol

    "should fail while accessing blockchain state from usual accounts" in {
      setAccountScript(bob, scriptHeightGt1)
      val order = mkOrder(bob, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      dex1.tryApi.place(order) should failWith(
        AccountScriptReturnedError.code,
        "The account's script of 3Q6ujVDbX57oLsXxifqfTcycgb4S8U3DLFz returned the error: An access to the blockchain.height is denied on DEX"
      )
    }

    "should succeed while accessing blockchain state from allowed accounts" in {
      setAccountScript(allowedAcc, scriptHeightGt1)
      val allowedAccOrder = mkOrder(allowedAcc, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      dex1.api.place(allowedAccOrder)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1, version = 2)
      placeAndAwaitAtNode(aliceOrder)
    }

    "transaction should be rejected by a new script which will be set after placing an order" in {
      setAccountScript(allowedAcc, scriptHeightGt1)
      val allowedAccOrder = mkOrder(allowedAcc, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      dex1.api.place(allowedAccOrder)
      setAccountScript(allowedAcc, scriptHeightGt1000)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1, version = 2)
      placeAndAwaitAtDex(aliceOrder, HttpOrderStatus.Status.Filled)
      val txId = ByteStr(dex1.api.getTransactionsByOrderId(allowedAccOrder).head.id().bytes())
      Thread.sleep(5.seconds.toMillis)
      wavesNode1.tryApi.transactionInfo(txId).isLeft shouldBe true
    }

    "order should be rejected by script" in {
      setAccountScript(allowedAcc, scriptHeightGt1000)
      val allowedAccOrder = mkOrder(allowedAcc, wavesUsdPair, OrderType.SELL, 10.waves, 1, version = 2)
      val placeResult = dex1.tryApi.place(allowedAccOrder)
      placeResult should failWith(
        AccountScriptDeniedOrder.code,
        "The account's script of 3Q97CnwDv7pE9wkYhEAq3juftoJiH1eaHGk rejected the order"
      )
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
    case _: SetScriptTransaction => true
    case _: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) && height > 1
    case _ => false
  }
   */
  private lazy val scriptHeightGt1 =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAAB" +
      "AAAAAgUAAAAHJG1hdGNoMAIAAAAFT3JkZXIDCQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4" +
      "AAAABnByb29mcwAAAAAAAAAAAAgFAAAAAnR4AAAAD3NlbmRlclB1YmxpY0tleQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAAAEHB7W9pT4="
    )

  /*
  {-# STDLIB_VERSION 5 #-}
  {-# CONTENT_TYPE EXPRESSION #-}
  {-# SCRIPT_TYPE ACCOUNT #-}
  match tx {
    case _: SetScriptTransaction => true
    case _: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) && height > 1000
    case _ => false
  }
   */
  private lazy val scriptHeightGt1000 =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAABAAA" +
      "AAgUAAAAHJG1hdGNoMAIAAAAFT3JkZXIDCQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnBy" +
      "b29mcwAAAAAAAAAAAAgFAAAAAnR4AAAAD3NlbmRlclB1YmxpY0tleQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAA+gHB4Yt2cc="
    )

  private def setAccountScript(account: KeyPair, script: ByteStr): Unit =
    broadcastAndAwait(mkSetAccountMayBeScript(account, Some(script), fee = setScriptFee + smartFee))

  private lazy val carol = mkKeyPair("carol")

}
