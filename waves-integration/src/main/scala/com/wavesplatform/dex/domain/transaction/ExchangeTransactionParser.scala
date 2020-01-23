package com.wavesplatform.dex.domain.transaction

import com.wavesplatform.dex.domain.bytes.deser.EntityParser

import scala.util.Try

trait ExchangeTransactionParser[T <: ExchangeTransaction] extends EntityParser[T] {

  protected def parseHeader(bytes: Array[Byte]): Try[Int]

  override def parseBytes(bytes: Array[Byte]): Try[T] = parseHeader(bytes) flatMap (offset => super.parseBytes(bytes drop offset))
}
