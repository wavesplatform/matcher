package com.wavesplatform.dex.domain.account

import com.google.common.collect.Interners
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.ByteStr._
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto._
import com.wavesplatform.dex.domain.error.ValidationError.InvalidAddress
import com.wavesplatform.dex.domain.utils.base58Length
import play.api.libs.json.{Format, Writes}
import supertagged._

object PublicKey extends TaggedType[ByteStr] {

  private[this] val interner = Interners.newWeakInterner[PublicKey]()

  val KeyStringLength: Int = base58Length(KeyLength)

  val empty: PublicKey = apply(ByteStr.empty)

  def apply(publicKey: ByteStr): PublicKey     = interner.intern(publicKey @@ this)
  def apply(publicKey: Array[Byte]): PublicKey = apply(ByteStr(publicKey))

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKey] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKey(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  def unapply(arg: Array[Byte]): Option[PublicKey] = Some(apply(arg))

  implicit def toAddress(pk: PublicKey): Address = pk.toAddress

  implicit class PublicKeyImplicitOps(private val pk: PublicKey) extends AnyVal {
    def toAddress: Address = Address.fromPublicKey(pk)
  }

  implicit lazy val jsonFormat: Format[PublicKey] = Format[PublicKey](
    byteStrFormat.map(this.apply),
    Writes(pk => byteStrFormat.writes(pk))
  )
}
