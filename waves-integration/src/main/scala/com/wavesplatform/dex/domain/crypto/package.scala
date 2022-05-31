package com.wavesplatform.dex.domain

import com.wavesplatform.dex.domain.account.{PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr

package object crypto {

  val SignatureLength: Int = Curve25519.SignatureLength
  val KeyLength: Int = Curve25519.KeyLength

  val DigestSize: Int = 32

  def fastHash(m: Array[Byte]): Array[Byte] = Blake2b256.hash(m)

  def secureHash(m: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(m))
  def secureHash(s: String): Array[Byte] = secureHash(s.getBytes("UTF-8"))

  def sign(account: PrivateKey, message: ByteStr): ByteStr = Curve25519.sign(account.arr, message)

  def verify(signature: ByteStr, message: ByteStr, publicKey: PublicKey): Boolean =
    Curve25519.verify(signature.arr, message, publicKey.arr)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = Curve25519.createKeyPair(seed)
}

import java.lang.reflect.Constructor
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

object Curve25519 {
  private lazy val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider].getDeclaredConstructors.head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  val KeyLength: Int = 32

  val SignatureLength: Int = 64

  def privateKeyFromSeed(seed: Array[Byte]): Array[Byte]            = provider.generatePrivateKey(Sha256.hash(seed))
  def publicKeyFromPrivateKey(privateKey: Array[Byte]): Array[Byte] = provider.generatePublicKey(privateKey)

  def createKeyPair(seed: Array[Byte]): (Array[Byte], Array[Byte]) = {
    val sk = privateKeyFromSeed(seed)
    val pk = publicKeyFromPrivateKey(sk)
    (sk, pk)
  }

  def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] =
    provider.calculateSignature(provider.getRandom(SignatureLength), privateKey, message)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean = provider.verifySignature(publicKey, message, signature)

}


import org.bouncycastle.crypto.Digest

abstract class BCDigest(initial: () => Digest, digestSize: Int) {
  protected val digest: ThreadLocal[Digest] = new ThreadLocal[Digest] {
    override def initialValue(): Digest = initial()
  }

  def hash(message: Array[Byte]): Array[Byte] = {
    val d = digest.get()
    d.update(message, 0, message.length)
    val result = new Array[Byte](digestSize)
    d.doFinal(result, 0)
    result
  }
}


import org.bouncycastle.crypto.digests.Blake2bDigest

object Blake2b256 extends BCDigest(() => new Blake2bDigest(256), 32)


import org.bouncycastle.crypto.digests.KeccakDigest

object Keccak256 extends BCDigest(() => new KeccakDigest(256), 32)

import java.security.MessageDigest

object Sha256 {
  private[this] val digest = new ThreadLocal[MessageDigest]() {
    override def initialValue(): MessageDigest = MessageDigest.getInstance("SHA-256")
  }

  def hash(message: Array[Byte]): Array[Byte] = {
    val result = digest.get().digest(message)
    digest.get().reset()
    result
  }
}
