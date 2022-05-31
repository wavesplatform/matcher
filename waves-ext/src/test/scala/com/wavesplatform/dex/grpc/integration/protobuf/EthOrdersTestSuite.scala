package com.wavesplatform.dex.grpc.integration.protobuf

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{Address, AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.WavesExtSuiteBase
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.it.api.BaseContainersKit
import com.wavesplatform.dex.it.api.dex.HasDex
import com.wavesplatform.dex.it.api.node.HasWavesNode
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.EthOrders.toEip712Json
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Proofs, TxVersion}
import org.scalatest.BeforeAndAfterAll
import org.web3j.crypto._
import org.web3j.utils.Numeric

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

final class EthOrdersTestSuite extends WavesExtSuiteBase with BaseContainersKit with HasWavesNode with HasDex with BeforeAndAfterAll {

  "EthOrdersTestSuite" - {

    "should update balances" in {
      println(alice)
    }
  }

  private lazy val aliceEthKeyPair = Bip32ECKeyPair.generateKeyPair(alice.seed)

  private lazy val aliceEthWavesAdr = Address(
    Numeric.hexStringToByteArray(Keys.getAddress(aliceEthKeyPair)),
    GenesisConfig.chainId
  )

  private lazy val bobEthKeyPair = Bip32ECKeyPair.generateKeyPair(bob.seed)

  private lazy val bobEthWavesAdr = Address(
    Numeric.hexStringToByteArray(Keys.getAddress(bobEthKeyPair)),
    GenesisConfig.chainId
  )

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
    val tr1 =
      TransferTransaction.selfSigned(
        TxVersion.V3,
        alice,
        aliceEthWavesAdr,
        Waves,
        15 * t8,
        Waves,
        300_000,
        ByteStr.empty,
        System.currentTimeMillis(),
        'Y'
      ).toOption.get

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

    val tr2 =
      TransferTransaction.selfSigned(
        TxVersion.V3,
        bob,
        bobEthWavesAdr,
        Asset.IssuedAsset(assetId),
        15 * t6,
        Waves,
        300_000,
        ByteStr.empty,
        System.currentTimeMillis(),
        'Y'
      ).toOption.get

    val tr3 =
      TransferTransaction.selfSigned(
        TxVersion.V3,
        bob,
        bobEthWavesAdr,
        Waves,
        15 * t8,
        Waves,
        300_000,
        ByteStr.empty,
        System.currentTimeMillis(),
        'Y'
      ).toOption.get

    println(assetId)
    val ap = AssetPair(Waves, Asset.IssuedAsset(assetId))

    import com.wavesplatform.transaction
    val order1 = signOrder(
      Order(
        Order.V4,
        OrderAuthentication.OrderProofs(PublicKey(new Array[Byte](32)), Proofs.empty),
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
      aliceEthKeyPair
    )

    val order2: Order = signOrder(
      Order(
        Order.V4,
        OrderAuthentication.OrderProofs(PublicKey(new Array[Byte](32)), Proofs.empty),
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
      bobEthKeyPair
    )

    val exchange = ExchangeTransaction.signed(
      TxVersion.V3,
      matcher.privateKey,
      order1,
      order2,
      5 * t8,
      100 * t6,
      300_000L,
      300_000L,
      300_000L,
      System.currentTimeMillis()
    )
      .fold(x => throw new RuntimeException(x.toString), identity)

    println("exId=" + exchange.id())

    wavesNode1.api.broadcastRaw(issue.json())
    Thread.sleep(3000L)
    wavesNode1.api.broadcastRaw(tr1.json())
    Thread.sleep(3000L)
    wavesNode1.api.broadcastRaw(tr2.json())
    Thread.sleep(3000L)
    wavesNode1.api.broadcastRaw(tr3.json())
    Thread.sleep(3000L)
    wavesNode1.api.broadcastRaw(exchange.json())
    Thread.sleep(3000L)
  }

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
