package com.wavesplatform.dex.it.collections

object Implicits {
  final implicit class ListOps[T](val self: List[T]) extends AnyVal {
    def prependIf(cond: Boolean)(item: => T): List[T] = if (cond) item :: self else self
  }
}
