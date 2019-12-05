package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import net.ceedubs.ficus.Ficus.traversableReader
import net.ceedubs.ficus.readers.ValueReader

object Implicits {

  implicit def nonEmptyListReader[T: ValueReader]: ValueReader[NonEmptyList[T]] = implicitly[ValueReader[List[T]]].map {
    case Nil     => throw new IllegalArgumentException("Expected at least one element")
    case x :: xs => NonEmptyList(x, xs)
  }
}
