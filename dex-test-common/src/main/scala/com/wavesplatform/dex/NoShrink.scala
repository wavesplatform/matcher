package com.wavesplatform.dex

import org.scalacheck.Shrink

trait NoShrink {
  implicit def noShrink[A]: Shrink[A] = Shrink.withLazyList(_ => LazyList.empty)
}
