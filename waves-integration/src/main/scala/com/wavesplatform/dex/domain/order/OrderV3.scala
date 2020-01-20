package com.wavesplatform.dex.domain.order

import cats.data.State
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.Stateful
import com.wavesplatform.dex.domain.bytes.{ByteStr, deser}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.{KeyLength, Proofs}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class OrderV3(senderPublicKey: PublicKey,
                   matcherPublicKey: PublicKey,
                   assetPair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timestamp: Long,
                   expiration: Long,
                   matcherFee: Long,
                   override val feeAsset: Asset,
                   proofs: Proofs)
    extends Order {

  def version: Byte = 3

  override def signature: Array[Byte] = proofs.proofs.head.arr

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(version),
        senderPublicKey,
        matcherPublicKey,
        assetPair.bytes,
        orderType.bytes,
        Longs.toByteArray(price),
        Longs.toByteArray(amount),
        Longs.toByteArray(timestamp),
        Longs.toByteArray(expiration),
        Longs.toByteArray(matcherFee),
        feeAsset.byteRepr
      )
    )

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      {
        val sig = Base58.encode(signature)
        Json.obj(
          "version"           -> version,
          "id"                -> idStr(),
          "sender"            -> senderPublicKey.stringRepr,
          "senderPublicKey"   -> Base58.encode(senderPublicKey),
          "matcherPublicKey"  -> Base58.encode(matcherPublicKey),
          "assetPair"         -> assetPair.json,
          "orderType"         -> orderType.toString,
          "amount"            -> amount,
          "price"             -> price,
          "timestamp"         -> timestamp,
          "expiration"        -> expiration,
          "matcherFee"        -> matcherFee,
          "matcherFeeAssetId" -> feeAsset.maybeBase58Repr,
          "signature"         -> sig,
          "proofs"            -> proofs.proofs
        )
      }
    )
}

object OrderV3 extends EntityParser[OrderV3] {

  private val AssetIdLength = 32

  def buy(sender: KeyPair,
          matcher: PublicKey,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long,
          matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(sender: KeyPair,
           matcher: PublicKey,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long,
           matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(sender: KeyPair,
            matcher: PublicKey,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            matcherFeeAssetId: Asset): Order = {

    val unsigned = OrderV3(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes1(bytes: Array[Byte]): Try[Order] = Try {

    val longLength = 8

    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }

    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }

    def readEnd[T](f: Array[Byte] => T): State[Int, T] = State { from =>
      (from, f(bytes.drop(from)))
    }

    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, from, size)
      (off, res)
    }

    val makeOrder = for {
      version <- readByte
      _ = if (version != 3) { throw new Exception(s"Incorrect order version: expect 3 but found $version") }
      sender            <- read(PublicKey.apply, KeyLength)
      matcher           <- read(PublicKey.apply, KeyLength)
      amountAssetId     <- parse(deser.parseByteArrayOption, AssetIdLength).map(arrOpt => Asset.fromCompatId(arrOpt.map(ByteStr(_))))
      priceAssetId      <- parse(deser.parseByteArrayOption, AssetIdLength).map(arrOpt => Asset.fromCompatId(arrOpt.map(ByteStr(_))))
      orderType         <- readByte
      price             <- read(Longs.fromByteArray, longLength)
      amount            <- read(Longs.fromByteArray, longLength)
      timestamp         <- read(Longs.fromByteArray, longLength)
      expiration        <- read(Longs.fromByteArray, longLength)
      matcherFee        <- read(Longs.fromByteArray, longLength)
      matcherFeeAssetId <- parse(deser.parseByteArrayOption, AssetIdLength).map(arrOpt => Asset.fromCompatId(arrOpt.map(ByteStr(_))))
      maybeProofs       <- readEnd(Proofs.fromBytes)
    } yield {
      OrderV3(
        senderPublicKey = sender,
        matcherPublicKey = matcher,
        assetPair = AssetPair(amountAssetId, priceAssetId),
        orderType = OrderType(orderType),
        amount = amount,
        price = price,
        timestamp = timestamp,
        expiration = expiration,
        matcherFee = matcherFee,
        feeAsset = matcherFeeAssetId,
        proofs = maybeProofs.right.get._1
      )
    }

    makeOrder.runA(0).value
  }

  override def statefulParse: Stateful[OrderV3] = {
    for {
      _           <- read[Byte].map(v => if (v != 3) throw new Exception(s"Incorrect order version: expect 3 but found $v"))
      sender      <- read[PublicKey]
      matcher     <- read[PublicKey]
      amountAsset <- read[Asset]
      priceAsset  <- read[Asset]
      orderType   <- read[Byte]
      price       <- read[Long]
      amount      <- read[Long]
      timestamp   <- read[Long]
      expiration  <- read[Long]
      matcherFee  <- read[Long]
      feeAsset    <- read[Asset]
      proofs      <- read[Proofs]
    } yield
      OrderV3(sender,
              matcher,
              AssetPair(amountAsset, priceAsset),
              OrderType(orderType),
              amount,
              price,
              timestamp,
              expiration,
              matcherFee,
              feeAsset,
              proofs)
  }
}
