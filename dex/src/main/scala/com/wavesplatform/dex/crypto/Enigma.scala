package com.wavesplatform.dex.crypto

import java.nio.charset.StandardCharsets
import java.security.NoSuchAlgorithmException
import java.security.spec.InvalidKeySpecException

import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKeyFactory}

import scala.util.control.NonFatal

object Enigma {

  private[this] val KeySalt           = "0495c728-1614-41f6-8ac3-966c22b4a62d".getBytes(StandardCharsets.UTF_8)
  private[this] val AES               = "AES"
  private[this] val Algorithm         = AES + "/ECB/PKCS5Padding"
  private[this] val HashingIterations = 999999
  private[this] val KeySizeBits       = 128

  def hashPassword(password: Array[Char],
                   salt: Array[Byte],
                   iterations: Int = HashingIterations,
                   keyLength: Int = KeySizeBits,
                   hashingAlgorithm: String = "PBKDF2WithHmacSHA512"): Array[Byte] =
    try {
      val keyFactory = SecretKeyFactory.getInstance(hashingAlgorithm)
      val keySpec    = new PBEKeySpec(password, salt, iterations, keyLength)
      val key        = keyFactory.generateSecret(keySpec)
      key.getEncoded
    } catch {
      case e @ (_: NoSuchAlgorithmException | _: InvalidKeySpecException) => throw new RuntimeException("Password hashing error", e)
    }

  def prepareDefaultKey(password: String): SecretKeySpec = new SecretKeySpec(hashPassword(password.toCharArray, KeySalt), AES)

  def encrypt(key: SecretKeySpec, bytes: Array[Byte]): Array[Byte] =
    try {
      val cipher = Cipher.getInstance(Algorithm)
      cipher.init(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(bytes)
    } catch {
      case NonFatal(e) => throw new RuntimeException("Encrypt error", e)
    }

  def decrypt(key: SecretKeySpec, encryptedBytes: Array[Byte]): Array[Byte] =
    try {
      val cipher: Cipher = Cipher.getInstance(Algorithm)
      cipher.init(Cipher.DECRYPT_MODE, key)
      cipher.doFinal(encryptedBytes)
    } catch {
      case NonFatal(e) => throw new RuntimeException("Decrypt error", e)
    }
}
