package com.wavesplatform.dex.settings.utils

import cats.implicits.catsSyntaxEitherId
import pureconfig.error.{ConfigReaderFailure, ConfigReaderFailures}
import pureconfig.generic.ProductHint
import pureconfig.{ConfigCursor, ConfigListCursor, ConfigObjectCursor, ConfigReader}

object ConfigReaderOps {

  implicit final class Implicits[T](val self: ConfigReader[T]) extends AnyVal {

    def validatedField(
      xs: (T, ConfigObjectCursor, ProductHint[T]) => Option[ConfigReaderFailure]*
    )(implicit productHint: ProductHint[T]): ConfigReader[T] = validated(_.asObjectCursor, xs)

    def validatedList(
      xs: (T, ConfigListCursor, ProductHint[T]) => Option[ConfigReaderFailure]*
    )(implicit productHint: ProductHint[T]): ConfigReader[T] = validated(_.asListCursor, xs)

    def validated[C <: ConfigCursor](
      toC: ConfigCursor => ConfigReader.Result[C],
      xs: Seq[(T, C, ProductHint[T]) => Option[ConfigReaderFailure]]
    )(implicit productHint: ProductHint[T]): ConfigReader[T] = self.flatMap { f =>
      ConfigReader.fromCursor[T] { c =>
        toC(c).flatMap { c =>
          val errors = xs.flatMap(_(f, c, productHint))
          if (errors.isEmpty) f.asRight
          else ConfigReaderFailures(errors.head, errors.tail: _*).asLeft
        }
      }
    }

  }

}
