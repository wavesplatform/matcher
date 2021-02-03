package com.wavesplatform.dex.domain.account

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.ByteStr._
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import play.api.libs.json.{Format, Json, Writes}

import scala.util.{Failure, Success}

final case class KeyPair(seed: ByteStr) {
  lazy val (privateKey, publicKey) = crypto.createKeyPair(seed) match {
    case (PrivateKey(privateKey), PublicKey(publicKey)) => (privateKey, publicKey)
    case _ => throw new IllegalArgumentException(s"Can't create key pair by provided seed=$seed")
  }
}

object KeyPair {

  def fromSeed(base58: String): Either[GenericError, KeyPair] = Base58.tryDecodeWithLimit(base58) match {
    case Success(x) => Right(KeyPair(x))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$base58': ${e.getMessage}"))
  }

  implicit class KeyPairImplicitOps(private val kp: KeyPair) extends AnyVal {
    def toAddress: Address = PublicKey.toAddress(kp)
  }

  implicit def toPublicKey(kp: KeyPair): PublicKey = kp.publicKey
  implicit def toPrivateKey(kp: KeyPair): PrivateKey = kp.privateKey
  implicit def toAddress(keyPair: KeyPair): Address = keyPair.toAddress

  implicit val jsonFormat: Format[KeyPair] = Format(
    byteStrFormat.map(KeyPair(_)),
    Writes(v => Json.obj("seed" -> Base58.encode(v.seed), "publicKey" -> v.publicKey, "privateKey" -> v.privateKey))
  )

}
