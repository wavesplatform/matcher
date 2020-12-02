package com.wavesplatform.dex.collections

// TODO DEX-994
object Implicits {

  implicit final class ListOps[T](val self: List[T]) extends AnyVal {
    def prependIf(cond: Boolean)(item: => T): List[T] = if (cond) item :: self else self
  }

}
