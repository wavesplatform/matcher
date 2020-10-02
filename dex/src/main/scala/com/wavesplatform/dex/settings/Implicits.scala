package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.wavesplatform.dex.domain.bytes.ByteStr
import net.ceedubs.ficus.Ficus.traversableReader
import net.ceedubs.ficus.readers.ValueReader
import pureconfig.ConfigReader

object Implicits {

  implicit def nonEmptyListReader[T: ValueReader]: ValueReader[NonEmptyList[T]] = implicitly[ValueReader[List[T]]].map {
    case Nil     => throw new IllegalArgumentException("Expected at least one element")
    case x :: xs => NonEmptyList(x, xs)
  }

  implicit val byteStrBase64ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase64)
}
