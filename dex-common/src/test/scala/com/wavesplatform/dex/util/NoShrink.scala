package com.wavesplatform.dex.util

import org.scalacheck.Shrink

trait NoShrink {
  // NoShrink
  implicit def noShrink[A]: Shrink[A] = Shrink.withLazyList(_ => LazyList.empty)
}
