package com.wavesplatform.dex.domain.bytes.deser

import cats.data.State
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.{deser, ByteStr}
import com.wavesplatform.dex.domain.crypto.{KeyLength, Proofs, SignatureLength}
import com.wavesplatform.dex.domain.utils._

import scala.util.Try

trait EntityParser[E] {

  import EntityParser._

  protected def read[R: Stateful]: Stateful[R] = implicitly

  private[domain] def statefulParse: Stateful[(E, ConsumedBytesOffset)]

  def parseBytes(bytes: Array[Byte]): Try[(E, ConsumedBytesOffset)] = Try(statefulParse.runA(S(0, bytes)).value)
}

object EntityParser {

  final private[domain] case class S(offset: Int, bytes: Array[Byte])
  final case class ConsumedBytesOffset(value: Int) extends AnyVal

  type Stateful[T] = State[S, T]
  type Signature = ByteStr

  private def standardRead[R](f: Array[Byte] => R, size: Int): Stateful[R] = State[S, R] { s =>
    s.copy(offset = s.offset + size) -> f(s.bytes.slice(s.offset, s.offset + size))
  }

  implicit val readByte: Stateful[Byte] = standardRead(_.head, 1)
  implicit val readInt: Stateful[Int] = standardRead(Ints.fromByteArray, 4)
  implicit val readLong: Stateful[Long] = standardRead(Longs.fromByteArray, 8)
  implicit val readPublicKey: Stateful[PublicKey] = standardRead(PublicKey.apply, KeyLength)
  implicit val readSignature: Stateful[Signature] = standardRead(ByteStr.apply, SignatureLength)

  implicit val readConsumedBytesOffset: Stateful[ConsumedBytesOffset] = State[S, ConsumedBytesOffset] { s =>
    s -> ConsumedBytesOffset(s.offset)
  }

  implicit val readProofs: Stateful[Proofs] = State[S, Proofs] { s =>
    val (proofs, length) = Proofs.fromBytes(s.bytes drop s.offset).explicitGet()
    s.copy(offset = s.offset + length) -> proofs
  }

  implicit val readAsset: Stateful[Asset] = State[S, Asset] { s =>
    val (maybeByteArr, resultOffset) = deser.parseByteArrayOption(s.bytes, s.offset, Asset.AssetIdLength)
    s.copy(offset = resultOffset) -> maybeByteArr.fold[Asset](Waves)(arr => IssuedAsset(ByteStr(arr)))
  }

}
