package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.WavesExtSuiteBase
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDex
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.EthOrders.toEip712Json
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.serialization.impl.ExchangeTxSerializer
import com.wavesplatform.transaction.{Asset, Proofs, TxVersion}
import org.scalatest.BeforeAndAfterAll
import org.web3j.crypto._

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

final class EthOrdersTestSuite extends WavesExtSuiteBase with BaseContainersKit with HasWavesNode with HasDex with BeforeAndAfterAll {

  "EthOrdersTestSuite" - {

    "should update balances" in {
      println(alice)
    }
  }

  private lazy val alice = KeyPair(
    crypto.secureHash(
      Bytes.concat(Ints.toByteArray(2), "seed".getBytes(StandardCharsets.UTF_8))
    )
  )

  private lazy val bob = KeyPair(
    crypto.secureHash(
      Bytes.concat(Ints.toByteArray(3), "seed".getBytes(StandardCharsets.UTF_8))
    )
  )

  private lazy val matcher = KeyPair(
    crypto.secureHash(
      Bytes.concat(Ints.toByteArray(0), "seed".getBytes(StandardCharsets.UTF_8))
    )
  )

  val t8 = 100_00L * 100_00L
  val t6 = 1000L * 1000L

  override protected def beforeAll(): Unit = {
    com.wavesplatform.account.AddressScheme.current = new AddressScheme {
      override val chainId: TxVersion = 'Y'
    }
    wavesNode1.start()
    dex1.start()

    val issue = IssueTransaction.selfSigned(
      TxVersion.V3,
      bob,
      "USD-N",
      "USD-N asset",
      999999999999L,
      6,
      false,
      None,
      1 * t8,
      System.currentTimeMillis()
    ).fold(x => throw new RuntimeException(x.toString), identity)
    val assetId = issue.asset.id

    val ap = AssetPair(Waves, Asset.IssuedAsset(assetId)) //8 6
    //10^6

    import com.wavesplatform.transaction

    val order1 = Order.sign(
      Order(
        Order.V3,
        OrderAuthentication.OrderProofs(alice.publicKey, Proofs.empty),
        matcher.publicKey,
        ap,
        OrderType.SELL,
        transaction.TxExchangeAmount.unsafeFrom(5 * t8),
        transaction.TxOrderPrice.unsafeFrom(100 * t6),
        System.currentTimeMillis(),
        System.currentTimeMillis() + 200_000L,
        transaction.TxMatcherFee.unsafeFrom(300_000L),
        Waves,
        OrderPriceMode.Default
      ),
      alice.privateKey
    )

    val order2: Order = Order.sign(
      Order(
        Order.V3,
        OrderAuthentication.OrderProofs(bob.publicKey, Proofs.empty),
        matcher.publicKey,
        ap,
        OrderType.BUY,
        transaction.TxExchangeAmount.unsafeFrom(5 * t8),
        transaction.TxOrderPrice.unsafeFrom(100 * t6),
        System.currentTimeMillis(),
        System.currentTimeMillis() + 200_000L,
        transaction.TxMatcherFee.unsafeFrom(300_000L),
        Waves,
        OrderPriceMode.Default
      ),
      bob.privateKey
    )

    val exchange = ExchangeTransaction.signed(
      TxVersion.V3,
      matcher.privateKey,
      order1,
      order2,
      5 * t8,
      convertPrice(100 * t6, 8, 6),
      300_000L,
      300_000L,
      300_000L,
      System.currentTimeMillis()
    )
      .fold(x => throw new RuntimeException(x.toString), identity)

    println("exId=" + exchange.id())

    wavesNode1.api.broadcastRaw(issue.json())
    Thread.sleep(3000L)
    wavesNode1.api.broadcastRaw(exchange.json())
    Thread.sleep(3000L)
    ExchangeTxSerializer
    println(wavesNode1)
  }

  def convertPrice(price: Long, amountDecimals: Int, priceDecimals: Int): Long =
    (BigDecimal(price) / BigDecimal(10).pow(priceDecimals - amountDecimals)).toBigInt.bigInteger.longValueExact()

  def hashOrderStruct(order: Order): Array[Byte] = {
    val json = toEip712Json(order)
    val encoder = new StructuredDataEncoder(json.toString)
    encoder.hashStructuredData()
  }

  def signOrder(order: Order, key: ECKeyPair): Order = {
    val message = hashOrderStruct(order)
    val signature = Sign.signMessage(message, key, false)
    val buffer = ByteBuffer.allocate(signature.getR.length + signature.getS.length + signature.getV.length)
    buffer.put(signature.getR)
    buffer.put(signature.getS)
    buffer.put(signature.getV)
    val sig = buffer.array()
    order.copy(orderAuthentication = OrderAuthentication.Eip712Signature(ByteStr(sig)))
  }

  override protected val moduleName: String = "m"
}
