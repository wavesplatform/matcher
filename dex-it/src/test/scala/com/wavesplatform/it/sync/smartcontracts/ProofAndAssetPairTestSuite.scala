package com.wavesplatform.it.sync.smartcontracts

import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{MatcherError, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV2}

import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {
  private val issueAliceAssetTx = mk(alice, "AliceCoin", someAssetAmount, 0)
  private val aliceAsset        = IssuedAsset(issueAliceAssetTx.id())

  private val predefAssetPair = wavesUsdPair
  private val aliceWavesPair  = AssetPair(aliceAsset, Waves)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(issueAliceAssetTx, IssueUsdTx)
  }

  "Proofs and AssetPairs verification with SmartContracts" - {
    val sc3 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPair1Amount = isDefined(t.assetPair.amountAsset)
                 |   let assetPair1Price = if (isDefined(t.assetPair.priceAsset)) then extract(t.assetPair.priceAsset) == base58'$UsdId' else false
                 |   let assetPair2Amount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'${issueAliceAssetTx
                   .id()}' else false
                 |   let assetPair2Price = isDefined(t.assetPair.priceAsset)
                 |   (!assetPair1Amount && assetPair1Price) || (assetPair2Amount && !assetPair2Price)
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sc4 = s"""
              |match tx {
              | case t : Order =>
              |    let id = t.id == base58''
              |    #let sender = t.sender == (base58'${ByteStr(alice.publicKey)}')
              |    let senderPublicKey = t.senderPublicKey == base58'${ByteStr(alice.publicKey)}'
              |    let matcherPublicKey = t.matcherPublicKey == base58'${ByteStr(matcher.publicKey)}'
              |    let timestamp = t.timestamp > 0
              |    let price = t.price > 0
              |    let amount = t.amount > 0
              |    let expiration = t.expiration > 0
              |    let matcherFee = t.matcherFee > 0
              |    let bodyBytes = t.bodyBytes == base64''
              |    !id && senderPublicKey && matcherPublicKey && timestamp && price && amount && expiration && matcherFee &&
              |    expiration && matcherFee && !bodyBytes
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sc5 = s"""
                 |match tx {
                 |  case t : Order =>
                 |        let pk1 = base58'${ByteStr(alice.publicKey)}'
                 |   sigVerify(t.bodyBytes,t.proofs[0],pk1)
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc6 = s"""
                 |match tx {
                 |  case t : Order =>
                 |        let pk1 = base58'${ByteStr(alice.publicKey)}'
                 |        let pk2 = base58'${ByteStr(bob.publicKey)}'
                 |        let pk3 = base58'${ByteStr(matcher.publicKey)}'
                 |
                 |        let alice = if (sigVerify(t.bodyBytes,t.proofs[0],pk1)) then 1 else 0
                 |        let bob = if (sigVerify(t.bodyBytes,t.proofs[1],pk2)) then 1 else 0
                 |        let matcher = if (sigVerify(t.bodyBytes,t.proofs[2],pk3)) then 1 else 0
                 |        alice + bob + matcher >=2
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc7 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPairAmount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'$WctId' else false
                 |   let assetPairPrice = isDefined(t.assetPair.priceAsset)
                 |   (assetPairAmount && !assetPairPrice)
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sc8 = s"""
                 |match tx {
                 |  case t : Order =>
                 |   let pk1 = base58'${ByteStr(matcher.publicKey)}' # here was alice
                 |   sigVerify(t.bodyBytes,t.proofs[0],pk1)
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    val sc9 = s"""
                 |match tx {
                 |  case t : Order => height < 0
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "positive scenarios of order placement" - {
      "set contracts with AssetPairs/all tx fields/true/one proof and then place order" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          setAliceScript(sc)

          val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd1)

          val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd2)

          dex1Api.cancel(alice, aliceOrd1)
          dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Cancelled)

          dex1Api.cancel(alice, aliceOrd2).status shouldBe "OrderCanceled"
          dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Cancelled)
        }
      }

      "set contracts with many proofs and then place order" - {
        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) s"$i" in {
          setAliceScript(sc)

          val currTime = System.currentTimeMillis()

          val unsigned =
            OrderV2(
              alice,
              matcher,
              predefAssetPair,
              OrderType.BUY,
              500,
              2.waves * Order.PriceConstant,
              currTime,
              (30.days - 1.seconds).toMillis + currTime,
              smartMatcherFee,
              Proofs.empty
            )

          val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
          val sigBob   = ByteStr(crypto.sign(bob, unsigned.bodyBytes()))

          val signed = unsigned.copy(proofs = Proofs(Seq(sigAlice, sigBob)))
          placeAndAwait(signed)

          dex1Api.cancel(alice, signed)
          dex1Api.waitForOrderStatus(signed, OrderStatus.Cancelled)
        }

        "reset" in resetAliceAccountScript()
      }

      "place order and then set contract on AssetPairs/true/all fields/one proof" - {
        for ((sc, i) <- Seq(sc1, sc3, sc4, sc5).zip(Seq(1, 3, 4, 5))) s"$i" in {
          log.debug(s"contract: $sc")
          val aliceOrd1 =
            mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd1)

          val aliceOrd2 =
            mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd2)

          setAliceScript(sc)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          dex1Api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, matcherFee = smartMatcherFee, version = 2)
          dex1Api.place(bobOrd2)

          dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(bobOrd1, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(bobOrd2, OrderStatus.Filled)

          waitForOrderAtNode(bobOrd1).fee shouldBe 300000
          waitForOrderAtNode(bobOrd2).fee shouldBe 300000

          dex1Api.reservedBalance(bob) shouldBe empty
        }
      }

      "place order and then set contract with many proofs" in {
        setAliceScript("true")
        broadcastAndAwait(mkTransfer(alice, bob, 1000, aliceAsset, 0.005.waves))

        for ((sc, i) <- Seq(sc5, sc6).zip(Seq(5, 6))) {
          markup(s"$i")

          for (assetP <- Seq(predefAssetPair, aliceWavesPair)) {
            val currTime = System.currentTimeMillis()

            val unsigned =
              OrderV2(
                alice,
                matcher,
                assetP,
                OrderType.BUY,
                500,
                2.waves * Order.PriceConstant,
                currTime,
                (30.days - 1.seconds).toMillis + currTime,
                smartMatcherFee,
                Proofs.empty
              )

            val sigAlice = ByteStr(crypto.sign(alice, unsigned.bodyBytes()))
            val sigMat   = ByteStr(crypto.sign(matcher.privateKey, unsigned.bodyBytes()))
            placeAndAwait(unsigned.copy(proofs = Proofs(Seq(sigAlice, ByteStr.empty, sigMat))))
          }

          setAliceScript(sc)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1Api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1Api.place(bobOrd2)

          dex1Api.waitForOrderStatus(bobOrd1, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(bobOrd2, OrderStatus.Filled)

          waitForOrderAtNode(bobOrd1).fee shouldBe 300000
          waitForOrderAtNode(bobOrd2).fee shouldBe 300000

          dex1Api.reservedBalance(bob) shouldBe empty
        }

        resetAliceAccountScript()
      }
    }

    "negative scenarios of order placement" - {
      "set contact and then place order" - {
        for ((sc, i) <- Seq(sc2, sc7, sc8).zip(Seq(2, 7, 8))) s"$i" in {
          log.debug(s"contract: $sc")
          setAliceScript(sc)

          dex1Api
            .tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147522,
            MatcherError.Params(address = Some(alice.toAddress.stringRepr))
          )

          dex1Api
            .tryPlace(mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147522,
            MatcherError.Params(address = Some(alice.toAddress.stringRepr))
          )
        }

        "9" in {
          setAliceScript(sc9)
          dex1Api.tryPlace(mkOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)) should failWith(
            3147521,
            "An access to the blockchain.height is denied on DEX"
          )
        }

        "reset" in resetAliceAccountScript()
      }

      "place order and then set contract" - {
        for ((contract, i) <- Seq(sc2, sc7, sc8, sc9).zip(Seq(2, 7, 8, 9))) s"$i" in {
          log.debug(s"contract $contract")

          val aliceOrd1 = mkOrder(alice, predefAssetPair, OrderType.BUY, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd1)

          val aliceOrd2 = mkOrder(alice, aliceWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          placeAndAwait(aliceOrd2)

          setAliceScript(contract)

          val bobOrd1 = mkOrder(bob, predefAssetPair, OrderType.SELL, 100, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1Api.place(bobOrd1)

          val bobOrd2 = mkOrder(bob, aliceWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant, smartMatcherFee, version = 2)
          dex1Api.place(bobOrd2)

          dex1Api.waitForOrderStatus(aliceOrd1, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(aliceOrd2, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(bobOrd1, OrderStatus.Filled)
          dex1Api.waitForOrderStatus(bobOrd2, OrderStatus.Filled)

          val aliceOrd1Txs = dex1Api.waitForTransactionsByOrder(aliceOrd1, 1)
          val r1           = wavesNode1Api.tryBroadcast(aliceOrd1Txs.head)
          r1 shouldBe 'left
          r1.left.get.error shouldBe TransactionNotAllowedByAccountScript.ErrorCode

          val aliceOrd2Txs = dex1Api.waitForTransactionsByOrder(aliceOrd2, 1)
          val r2           = wavesNode1Api.tryBroadcast(aliceOrd2Txs.head)
          r2 shouldBe 'left
          r2.left.get.error shouldBe TransactionNotAllowedByAccountScript.ErrorCode

          dex1Api.orderHistoryWithApiKey(alice, activeOnly = Some(true)).length shouldBe 0
          dex1Api.reservedBalance(bob) shouldBe empty

          resetAliceAccountScript()
        }
      }
    }
  }

  private def setAliceScript(scriptText: String): Unit = broadcastAndAwait(mkSetAccountScriptText(alice, Some(scriptText), fee = setScriptFee + smartFee))
  private def resetAliceAccountScript(): Unit          = broadcastAndAwait(mkSetAccountScriptText(alice, None, fee = setScriptFee + smartFee))
}
