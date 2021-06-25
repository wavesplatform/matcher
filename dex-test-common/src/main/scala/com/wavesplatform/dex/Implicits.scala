package com.wavesplatform.dex

import org.scalatest.concurrent.PatienceConfiguration.Timeout

import scala.concurrent.duration.Duration
import scala.util.Using.Releasable

object Implicits {

  implicit def durationToScalatestTimeout(d: Duration): Timeout = Timeout(d)

  implicit def releasable[T <: AutoCloseable]: Releasable[Seq[T]] = seq => seq.foreach(v => v.close())

}
