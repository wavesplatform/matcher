package com.wavesplatform.dex.settings.utils

import cats.implicits.catsSyntaxEitherId
import pureconfig.error.{ConfigReaderFailure, ConfigReaderFailures}
import pureconfig.{ConfigCursor, ConfigReader}

object ConfigReaderOps {
  implicit final class ConfigReaderMyOps[T](val self: ConfigReader[T]) extends AnyVal {
    def validated(xs: (T, ConfigCursor) => Option[ConfigReaderFailure]*): ConfigReader[T] = self.flatMap { f =>
      ConfigReader.fromCursor[T] { c =>
        val errors = xs.flatMap(_(f, c))
        if (errors.isEmpty) f.asRight
        else ConfigReaderFailures(errors.head, errors.tail: _*).asLeft
      }
    }
  }
}
