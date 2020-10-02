package com.wavesplatform.dex.settings.utils

import cats.implicits.catsSyntaxEitherId
import pureconfig.error.{ConfigReaderFailure, ConfigReaderFailures}
import pureconfig.generic.ProductHint
import pureconfig.{ConfigObjectCursor, ConfigReader}

object ConfigReaderOps {
  implicit final class ConfigReaderMyOps[T](val self: ConfigReader[T]) extends AnyVal {
    def validated(
        xs: (T, ConfigObjectCursor, ProductHint[T]) => Option[ConfigReaderFailure]*
    )(implicit productHint: ProductHint[T]): ConfigReader[T] = self.flatMap { f =>
      ConfigReader.fromCursor[T] { c =>
        // Validation works for objects only right now
        c.asObjectCursor.flatMap { c =>
          val errors = xs.flatMap(_(f, c, productHint))
          if (errors.isEmpty) f.asRight
          else ConfigReaderFailures(errors.head, errors.tail: _*).asLeft
        }
      }
    }
  }
}
