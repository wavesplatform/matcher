package com.wavesplatform.it.api

import cats.MonadError

package object dex {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]
}
