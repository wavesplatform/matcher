package com.wavesplatform.dex.domain

import com.wavesplatform.dex.domain.account.{PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import scorex.crypto.hash.{Blake2b256, Keccak256}
import scorex.crypto.signatures.{Curve25519, Signature, PrivateKey => SPrivateKey, PublicKey => SPublicKey}

package object crypto {

  val SignatureLength: Int = Curve25519.SignatureLength
  val KeyLength: Int       = Curve25519.KeyLength

  val DigestSize: Int = 32

  def fastHash(m: Array[Byte]): Array[Byte] = Blake2b256.hash(m)

  def secureHash(m: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(m))
  def secureHash(s: String): Array[Byte]      = secureHash(s.getBytes("UTF-8"))

  def sign(account: PrivateKey, message: ByteStr): ByteStr = Curve25519.sign(SPrivateKey(account.arr), message)

  def verify(signature: ByteStr, message: ByteStr, publicKey: PublicKey): Boolean = {
    Curve25519.verify(Signature(signature.arr), message, SPublicKey(publicKey.arr))
  }

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = Curve25519.createKeyPair(seed)
}
