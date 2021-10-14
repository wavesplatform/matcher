package com.wavesplatform.it.sync

import cats.syntax.either._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.AccountScriptReturnedError
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.model.ExecutionParamsInProofs
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transactions.common.Proof
import com.wavesplatform.transactions.exchange

import scala.jdk.CollectionConverters._

final class OrderExecutedDataInProofsTestSuite extends MatcherSuiteBase {

  private val orderVersion = 3.toByte

  "OrderExecutedDataInProofs" - {

    "before offset don't add extra data for any account" in {
      val aliceOrder = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 1.waves, 2.usdn, version = orderVersion)
      dex1.api.place(aliceOrder)
      val carolOrder = mkOrder(carol, wavesUsdnPair, OrderType.BUY, 1.waves, 2.usdn, version = orderVersion)
      dex1.api.place(carolOrder)

      dex1.api.waitForOrderStatus(carolOrder, HttpOrderStatus.Status.Filled)

      val txs = dex1.api.getTransactionsByOrderId(carolOrder)
      txs.size shouldBe 1
      val orders = txs.head.orders()
      orders.size() shouldBe 2
      orders.asScala.foreach(_.proofs().size() shouldBe 1)

      wavesNode1.api.waitForHeightArise()
    }

    "add extra data for old order if offset is right" in {
      val bobOrder = mkOrder(bob, wavesUsdnPair, OrderType.BUY, 5.waves, 2.usdn, version = orderVersion)
      dex1.api.place(bobOrder)

      val aliceOrder1 = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 4.3.waves, 2.usdn, version = orderVersion)
      dex1.api.place(aliceOrder1)

      val txs1 = dex1.api.getTransactionsByOrderId(aliceOrder1)
      txs1.size shouldBe 1
      val orders1 = txs1.head.orders()
      orders1.size() shouldBe 2
      orders1.asScala.foreach(_.proofs().size() shouldBe 1)

      wavesNode1.api.waitForHeightArise()

      broadcastAndAwait(
        mkSetAccountMayBeScript(alice, Some(script), fee = setScriptFee + smartFee),
        mkSetAccountMayBeScript(bob, Some(script), fee = setScriptFee + smartFee)
      )

      val aliceOrder2 = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 0.7.waves, 1.3.usdn, version = orderVersion)
      dex1.api.place(aliceOrder2)

      val txs2 = dex1.api.getTransactionsByOrderId(aliceOrder2)
      txs2.size shouldBe 1
      val orders2 = txs2.head.orders()
      orders2.size() shouldBe 2
      orders2.asScala.foreach(order => checkProofs(order.proofs.asScala.toList, 0.7.waves, 2.usdn))
    }

    "after offset add extra data only for account from config" in {
      val aliceOrder = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 1.waves, 2.usdn, version = orderVersion)
      dex1.api.place(aliceOrder)
      val carolOrder = mkOrder(carol, wavesUsdnPair, OrderType.BUY, 1.waves, 2.usdn, version = orderVersion)
      dex1.api.place(carolOrder)

      dex1.api.waitForOrderStatus(carolOrder, HttpOrderStatus.Status.Filled)

      val txs = dex1.api.getTransactionsByOrderId(carolOrder)
      txs.size shouldBe 1
      val orders = txs.head.orders()
      orders.size() shouldBe 2
      orders.asScala.foreach { order =>
        if (order.`type`() == exchange.OrderType.BUY) order.proofs().size() shouldBe 1
        else checkProofs(order.proofs.asScala.toList, 1.waves, 2.usdn)
      }
    }

    "after offset add extra data for both orders if their senders are in config" in {
      val aliceOrder = mkOrder(alice, wavesUsdnPair, OrderType.BUY, 5.waves, 2.usdn, version = orderVersion)
      dex1.api.place(aliceOrder)
      val bobOrder = mkOrder(bob, wavesUsdnPair, OrderType.SELL, 5.waves, 2.usdn, version = orderVersion)
      dex1.api.place(bobOrder)

      dex1.api.waitForOrderStatus(bobOrder, HttpOrderStatus.Status.Filled)

      val txs = dex1.api.getTransactionsByOrderId(bobOrder)
      txs.size shouldBe 1
      val orders = txs.head.orders()
      orders.size() shouldBe 2
      orders.asScala.foreach(order => checkProofs(order.proofs.asScala.toList, 5.waves, 2.usdn))
    }

    "with script" - {
      "fail Alice's order" in {
        val failOrder = mkOrder(alice, wavesUsdnPair, OrderType.BUY, 200.waves, 150.usdn, version = orderVersion)
        dex1.tryApi.place(failOrder).leftMap(_.error) shouldBe Left(AccountScriptReturnedError.code)
      }

      "pass valid Alice's order" in {
        val okOrder = mkOrder(alice, wavesUsdnPair, OrderType.BUY, 2.waves, 3.usdn, version = orderVersion)
        dex1.tryApi.place(okOrder) shouldBe a[Right[_, _]]
      }
    }

  }

  protected val carol = mkKeyPair("carol")

  private def checkProofs(proofs: List[Proof], execAmount: Long, execPrice: Long): Unit = {
    proofs.size shouldBe 3

    val amountProof = ExecutionParamsInProofs.encodeToBytes(execAmount, 8)
    val priceProof = ExecutionParamsInProofs.encodeToBytes(execPrice, 8)

    proofs(1).encoded() shouldBe amountProof.base58
    proofs(2).encoded() shouldBe priceProof.base58
  }

  override def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdnTx)
    broadcastAndAwait(
      mkTransfer(alice, carol, defaultAssetQuantity / 3, usdn),
      mkTransfer(alice, bob, defaultAssetQuantity / 3, usdn),
      mkTransfer(bob, carol, 500.waves, Waves)
    )
    dex1.start()
  }

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdnId", "WAVES" ]
       |  pass-execution-parameters {
       |    since-offset = 3
       |    for-accounts = [${alice.publicKey}, ${bob.publicKey}]
       |  }
       |}""".stripMargin
  )

  override protected def wavesNodeInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""
         |waves.dex.order-script-validation.allowed-blockchain-state-accounts=["${alice.publicKey.base58}", "${bob.publicKey.base58}"]
         |""".stripMargin
    )

  /*
  {-# STDLIB_VERSION 5 #-}
  {-# CONTENT_TYPE EXPRESSION #-}
  {-# SCRIPT_TYPE ACCOUNT #-}

  match tx {
      case o: Order =>
          let orderPriceConstant = 100_000_000 # 10^8
          let execPrice = toInt(o.proofs[2])
          let execA = toInt(o.proofs[1])
          let execB = execPrice * execA / orderPriceConstant

          if (execB > 10000000) then {
              throw(makeString(["execPrice=", toString(execPrice), "execA=", toString(execA), "execB=", toString(execB)], ", "))
          } else {
              true
          }

      case tx => sigVerify_128Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
  }
   */
  private val script = Scripts.fromBase64(
    "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAABU9yZGVyBAAAAAFvBQAAAAckbWF0Y2gwBAAAABJvcmRlclByaW" +
    "NlQ29uc3RhbnQAAAAAAAX14QAEAAAACWV4ZWNQcmljZQkABLEAAAABCQABkQAAAAIIBQAAAAFvAAAABnByb29mcwAAAAAAAAAAAgQAAAAFZXhl" +
    "Y0EJAASxAAAAAQkAAZEAAAACCAUAAAABbwAAAAZwcm9vZnMAAAAAAAAAAAEEAAAABWV4ZWNCCQAAaQAAAAIJAABoAAAAAgUAAAAJZXhlY1ByaW" +
    "NlBQAAAAVleGVjQQUAAAASb3JkZXJQcmljZUNvbnN0YW50AwkAAGYAAAACBQAAAAVleGVjQgAAAAAAAJiWgAkAAAIAAAABCQAEuQAAAAIJAARM" +
    "AAAAAgIAAAAKZXhlY1ByaWNlPQkABEwAAAACCQABpAAAAAEFAAAACWV4ZWNQcmljZQkABEwAAAACAgAAAAZleGVjQT0JAARMAAAAAgkAAaQAAA" +
    "ABBQAAAAVleGVjQQkABEwAAAACAgAAAAZleGVjQj0JAARMAAAAAgkAAaQAAAABBQAAAAVleGVjQgUAAAADbmlsAgAAAAIsIAYEAAAAAnR4BQAA" +
    "AAckbWF0Y2gwCQAJyAAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnByb29mcwAAAAAAAAAAAAgFAAAAAnR4AA" +
    "AAD3NlbmRlclB1YmxpY0tledvMS2Y="
  )

}
