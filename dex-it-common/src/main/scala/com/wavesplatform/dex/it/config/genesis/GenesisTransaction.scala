package com.wavesplatform.dex.it.config.genesis

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.error.ValidationError
import monix.eval.Coeval

case class GenesisTransaction(recipient: Address, amount: Long, timestamp: Long, signature: ByteStr) {

  import GenesisTransaction._

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {

    val typeBytes      = Array(typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val rcpBytes       = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)

    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == TypeLength + BASE_LENGTH)

    res
  }

  val bodyBytes: Coeval[Array[Byte]] = bytes
}

object GenesisTransaction {

  val typeId: Byte = 1

  private val TimestampLength, AmountLength = 8
  private val TypeLength                    = 1

  private val RECIPIENT_LENGTH = Address.AddressLength
  private val BASE_LENGTH      = TimestampLength + RECIPIENT_LENGTH + AmountLength

//  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
//
//    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(typeId), TypeLength, 0)
//    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
//    val amountBytes    = Longs.toByteArray(amount)
//    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)
//
//    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))
//
//    val h = crypto.fastHash(data)
//    Bytes.concat(h, h)
//  }
//
//  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
//    if (amount < 0) Left(ValidationError.NonPositiveAmount(amount, "waves"))
//    else {
//      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
//      Right(GenesisTransaction(recipient, amount, timestamp, signature))
//    }
//  }
}
