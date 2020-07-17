package com.wavesplatform.dex

import org.scalacheck.Shrink

import scala.annotation.nowarn

@nowarn
trait NoShrink {
  implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
}
