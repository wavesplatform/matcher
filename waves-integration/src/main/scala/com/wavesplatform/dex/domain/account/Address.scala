package com.wavesplatform.dex.domain.account

import java.nio.ByteBuffer

import cats.syntax.either._
import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.error.ValidationError.InvalidAddress
import com.wavesplatform.dex.domain.utils.{base58Length, ScorexLogging}
import play.api.libs.json._

sealed trait Address extends AddressOrAlias {
  val bytes: ByteStr
  lazy val stringRepr: String = bytes.base58
}

//noinspection ScalaDeprecation
object Address extends ScorexLogging {

  val Prefix: String = "address:"
  val AddressVersion: Byte = 1
  val ChecksumLength: Int = 4
  val HashLength: Int = 20
  val AddressLength: Int = 1 + 1 + HashLength + ChecksumLength
  val AddressStringLength: Int = base58Length(AddressLength)

  private[this] val publicKeyBytesCache: Cache[ByteStr, Address] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  private[this] val bytesCache: Cache[ByteStr, Either[InvalidAddress, Address]] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  def fromPublicKey(publicKey: PublicKey, chainId: Byte = scheme.chainId): Address =
    publicKeyBytesCache.get(
      publicKey,
      { () =>
        val withoutChecksum = ByteBuffer
          .allocate(1 + 1 + HashLength)
          .put(AddressVersion)
          .put(chainId)
          .put(crypto.secureHash(publicKey), 0, HashLength)
          .array()

        val bytes = ByteBuffer
          .allocate(AddressLength)
          .put(withoutChecksum)
          .put(calcCheckSum(withoutChecksum), 0, ChecksumLength)
          .array()

        createUnsafe(bytes)
      }
    )

  def fromBytes(addressBytes: ByteStr, chainId: Byte = scheme.chainId): Either[InvalidAddress, Address] =
    bytesCache.get(
      addressBytes,
      () =>
        Either
          .cond(
            addressBytes.length == Address.AddressLength,
            (),
            InvalidAddress(s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
          )
          .flatMap {
            _ =>
              val (version, network) = addressBytes.arr match {
                case Array(version, network, _*) => (version, network)
                case _ => throw new IllegalArgumentException(s"Can't process order bytes of len=${addressBytes.size}, there should be both version and network")
              }
              (
                for {
                  _ <- Either.cond(version == AddressVersion, (), s"Unknown address version: $version")
                  _ <- Either.cond(
                    network == chainId,
                    (),
                    s"Data from other network: expected: $chainId(${chainId.toChar}), actual: $network(${network.toChar})"
                  )
                  checkSum = addressBytes.takeRight(ChecksumLength)
                  checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
                  _ <- Either.cond(java.util.Arrays.equals(checkSum, checkSumGenerated), (), s"Bad address checksum")
                } yield createUnsafe(addressBytes)
              ).leftMap(err => InvalidAddress(err))
          }
    )

  def fromString(addressStr: String): Either[InvalidAddress, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(
        base58String.length <= AddressStringLength,
        (),
        InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}")
      )
      byteArray <- Base58.tryDecodeWithLimit(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address <- fromBytes(byteArray)
    } yield address
  }

  def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = {
    val fullHash = crypto.secureHash(withoutChecksum)
    fullHash.take(ChecksumLength)
  }

  implicit val jsonFormat: Format[Address] = Format[Address](
    Reads(jsValue => fromString(jsValue.as[String]).fold(err => JsError(err.toString), JsSuccess(_))),
    Writes(addr => JsString(addr.stringRepr))
  )

  @inline
  private[this] def scheme: AddressScheme = AddressScheme.current

  // Optimization, should not be used externally
  private def createUnsafe(address: ByteStr): Address = AddressImpl(address)
  final private case class AddressImpl(bytes: ByteStr) extends Address
}
