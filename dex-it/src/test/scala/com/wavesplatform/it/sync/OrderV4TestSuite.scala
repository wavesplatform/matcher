package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.{EthOrders, Order, OrderAuthentication, OrderType}
import com.wavesplatform.dex.error.{InvalidJson, OrderCommonValidationFailed, OrderInvalidSignature, UnsupportedOrderVersion}
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.model.AcceptedOrder
import com.wavesplatform.it.MatcherSuiteBase
import org.web3j.crypto.{Bip32ECKeyPair, ECKeyPair, Keys}
import org.web3j.utils.Numeric
import play.api.libs.json.{JsString, Json}

import scala.concurrent.duration._

class OrderV4TestSuite extends MatcherSuiteBase {

  //will be the same for waves & usd, see rate
  private val fee = 300_000L

  private val eip712SignatureSample =
    "0x9dfd57cfdb30d4a0fa9a6853f29c6e010e24c425fce228a5c3316596d7f88eea5a8fc8278d2c13f28df913b723ac07d82dce0d74121da042f7ec4275d301df2a1b"

  private val aliceEth = Bip32ECKeyPair.generateKeyPair(alice.seed)

  private val aliceEthAdr = Address.fromPublicKeyHash(
    Numeric.hexStringToByteArray(Keys.getAddress(aliceEth)),
    GenesisConfig.chainId
  )

  private val bobEth = Bip32ECKeyPair.generateKeyPair(bob.seed)

  private val bobEthAdr = Address.fromPublicKeyHash(
    Numeric.hexStringToByteArray(Keys.getAddress(bobEth)),
    GenesisConfig.chainId
  )

  "OrderV4TestSuite" - {

    "should work with proofs authentication" in {
      test(alice, bob, f => sign(alice, f), f => sign(bob, f), orderVersion = 4)
    }

    "should work with eip712Signature authentication" in {
      test(aliceEthAdr, bobEthAdr, f => signEth(aliceEth, f), f => signEth(bobEth, f), orderVersion = 4)
    }

    "should work with mixed authentication" in {
      test(alice, bobEthAdr, f => sign(alice, f), f => signEth(bobEth, f), orderVersion = 4)
    }

    "should work with start offset" in {
      val order = sign(alice, mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, 5.usd, Waves, version = 4))
      dex1.restartWithNewSuiteConfig(suiteInitialConfig(orderV4StartOffset = 1L))
      dex1.tryApi.place(order) should failWith(UnsupportedOrderVersion.code)
      test(alice, bob, f => sign(alice, f), f => sign(bob, f), orderVersion = 3)
      test(alice, bob, f => sign(alice, f), f => sign(bob, f), orderVersion = 4)
    }

    "should reject invalid eip712Signature" in {
      val order = signEth(aliceEth, mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, 5.usd, Waves, version = 4))
      val orderJson = order.json() ++ Json.obj(
        "eip712Signature" -> JsString(eip712SignatureSample)
      )
      dex1.tryApi.place(orderJson) should failWith(OrderInvalidSignature.code)
    }

    "should reject corrupted eip712Signature" in {
      val order = signEth(aliceEth, mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, 5.usd, Waves, version = 4))
      val orderJson = order.json() ++ Json.obj(
        "eip712Signature" -> JsString("corrupted")
      )
      dex1.tryApi.place(orderJson) should failWith(OrderInvalidSignature.code)
    }

    "should reject order with price that doesn't fit into FIXED_DECIMALS" in {
      val price = 2L * Order.PriceConstant * Order.PriceConstant
      val order = sign(alice, mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, price, Waves, version = 4))
      dex1.tryApi.place(order) should failWith(
        OrderCommonValidationFailed.code,
        "The order is invalid: Price is not convertible to fixed decimals format"
      )
    }

    "should reject order without sender, proofs and eip712Signature" in {
      val order = sign(alice, mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, 5.usd, Waves, version = 4))
      val orderJson = {
        order.json() - "eip712Signature" - "sender" - "senderPublicKey" - "proofs" - "signature"
      }
      dex1.tryApi.place(orderJson) should failWith(InvalidJson.code)
    }
  }

  private def test(
    buyer: Address,
    seller: Address,
    buySign: (OrderAuthentication => Order) => Order,
    sellSign: (OrderAuthentication => Order) => Order,
    orderVersion: Byte
  ): Unit = {
    val buyerWaves = wavesNode1.api.wavesBalance(buyer)
    val buyerUsd = wavesNode1.api.assetBalance(buyer, usd)
    val sellerWaves = wavesNode1.api.wavesBalance(seller)
    val sellerUsd = wavesNode1.api.assetBalance(seller, usd)

    val buy = buySign(mkOrder(wavesUsdPair, OrderType.BUY, 10.waves, 5.usd, Waves, orderVersion))
    val sell = sellSign(mkOrder(wavesUsdPair, OrderType.SELL, 10.waves, 5.usd, usd, orderVersion))

    placeAndAwaitAtDex(buy)
    val txs = placeAndAwaitAtNode(sell)

    val spendUsd = AcceptedOrder.calcAmountOfPriceAsset(10.waves, 5.usd)
    buyerWaves + 10.waves - fee shouldBe wavesNode1.api.wavesBalance(buyer)
    buyerUsd - spendUsd shouldBe wavesNode1.api.assetBalance(buyer, usd)
    sellerWaves - 10.waves shouldBe wavesNode1.api.wavesBalance(seller)
    sellerUsd + spendUsd - fee shouldBe wavesNode1.api.assetBalance(seller, usd)

    txs should not be empty
    txs.foreach { tx =>
      if (orderVersion == 4)
        tx.version() shouldBe 3
      else
        tx.version() shouldBe 2
    }
  }

  private def sign(signer: KeyPair, f: OrderAuthentication => Order): Order = {
    val proofs = OrderAuthentication.OrderProofs(signer, Proofs.empty)
    Order.sign(f(proofs), signer)
  }

  private def signEth(signer: ECKeyPair, f: OrderAuthentication => Order): Order = {
    val sig = OrderAuthentication.Eip712Signature(ByteStr.empty)
    EthOrders.signOrder(f(sig), signer)
  }

  private def mkOrder(
    assetPair: AssetPair,
    orderType: OrderType,
    amount: Long,
    price: Long,
    feeAsset: Asset,
    version: Byte
  ): OrderAuthentication => Order =
    (oa: OrderAuthentication) => {
      val ts = System.currentTimeMillis
      Order(
        oa,
        matcher,
        assetPair,
        orderType,
        amount,
        price,
        ts,
        ts + (30.days - 1.seconds).toMillis,
        fee,
        version,
        feeAsset
      )
    }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(
      mkTransfer(alice, aliceEthAdr, 10_000.waves, Waves),
      mkTransfer(alice, aliceEthAdr, 100_000.usd, usd),
      mkTransfer(alice, bobEthAdr, 10_000.waves, Waves),
      mkTransfer(alice, bobEthAdr, 100_000.usd, usd),
      mkTransfer(alice, bob, 100_000.usd, usd)
    )
    dex1.start()
    dex1.api.upsertAssetRate(usd, 1000000.0)
  }

  override protected def dexInitialSuiteConfig: Config = suiteInitialConfig(orderV4StartOffset = -1L)

  private def suiteInitialConfig(orderV4StartOffset: Long): Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  allowed-order-versions = [3,4]
       |  order-v-4-start-offset = $orderV4StartOffset
       |  price-assets = [ "$UsdId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      default {
       |        mode = dynamic
       |        dynamic {
       |          base-maker-fee = 300000
       |          base-taker-fee = 300000
       |        }
       |      }
       |
       |      discount {
       |        asset = "$usd"
       |        value = 0
       |      }
       |    }
       |  }
       |}""".stripMargin
  )

}
