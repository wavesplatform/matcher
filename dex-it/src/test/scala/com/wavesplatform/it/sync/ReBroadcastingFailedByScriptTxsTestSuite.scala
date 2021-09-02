package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.duration._

final class ReBroadcastingFailedByScriptTxsTestSuite extends MatcherSuiteBase {

  "ReBroadcastingFailedByScriptTxsTestSuite" - {

    "should not rebroadcast transactions failed by account script" in {
      val carolOrder = mkOrder(carol, wavesUsdPair, OrderType.SELL, 10.waves, 10.usd, version = 2)
      placeAndAwaitAtDex(carolOrder)
      setAccountScript(carol, scriptHeightGt1000)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 10.usd, version = 2)
      placeAndAwaitAtDex(aliceOrder, HttpOrderStatus.Status.Filled)
      Thread.sleep(5.seconds.toMillis)
      val txId = ByteStr(dex1.api.getTransactionsByOrderId(carolOrder).head.id().bytes())
      wavesNode1.tryApi.transactionInfo(txId).isLeft shouldBe true
      setAccountScript(carol, defaultScript)
      Thread.sleep(5.seconds.toMillis)
      wavesNode1.tryApi.transactionInfo(txId).isLeft shouldBe true
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
         |  exchange-transaction-broadcast {
         |    interval = 1 second
         |    max-pending-time = 15 minutes
         |  }
         |}""".stripMargin
    )

  override protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""
         |waves.dex.order-script-validation.allowed-blockchain-state-accounts=["${carol.publicKey.base58}"]
         |""".stripMargin
    )

  private def setAccountScript(account: KeyPair, script: ByteStr): Unit =
    broadcastAndAwait(mkSetAccountMayBeScript(account, Some(script), fee = setScriptFee + smartFee))

  private lazy val carol = mkKeyPair("carol")

  /*
  {-# STDLIB_VERSION 5 #-}
  {-# CONTENT_TYPE EXPRESSION #-}
  {-# SCRIPT_TYPE ACCOUNT #-}

  match (tx) {
    case _: SetScriptTransaction => true
    case _: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
    case _ => false
  }
   */
  private lazy val defaultScript =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAABAAAAA" +
      "gUAAAAHJG1hdGNoMAIAAAAFT3JkZXIJAAH0AAAAAwgFAAAAAnR4AAAACWJvZHlCeXRlcwkAAZEAAAACCAUAAAACdHgAAAAGcHJv" +
      "b2ZzAAAAAAAAAAAACAUAAAACdHgAAAAPc2VuZGVyUHVibGljS2V5BwPe+og="
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
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAABAAAAAgUAA" +
      "AAHJG1hdGNoMAIAAAAFT3JkZXIDCQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnByb29mcwAAA" +
      "AAAAAAAAAgFAAAAAnR4AAAAD3NlbmRlclB1YmxpY0tleQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAA+gHB4Yt2cc="
    )

}
