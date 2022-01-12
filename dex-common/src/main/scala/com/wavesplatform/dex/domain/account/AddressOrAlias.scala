package com.wavesplatform.dex.domain.account

import com.wavesplatform.dex.domain.bytes.{deser, ByteStr}
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.error.ValidationError._

trait AddressOrAlias {

  def stringRepr: String
  def bytes: ByteStr

  override def toString: String = stringRepr

  def canEqual(other: Any): Boolean = other.isInstanceOf[AddressOrAlias]

  override def equals(obj: Any): Boolean = obj match {
    case a: AddressOrAlias => a.canEqual(this) && bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object AddressOrAlias {

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (AddressOrAlias, Int)] = bytes(position) match {
    case Address.AddressVersion =>
      val addressEnd = position + Address.AddressLength
      val addressBytes = bytes.slice(position, addressEnd)
      Address.fromBytes(addressBytes).map((_, addressEnd))
    case Alias.AddressVersion =>
      val (_, aliasEnd) = deser.parseArrayWithLength(bytes, position + 2)
      Alias.fromBytes(bytes.slice(position, aliasEnd)).map((_, aliasEnd))
    case _ => Left(InvalidAddress("Unknown address/alias version"))
  }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] =
    if (s.startsWith(Alias.Prefix)) Alias.fromString(s)
    else Address.fromString(s)

}
