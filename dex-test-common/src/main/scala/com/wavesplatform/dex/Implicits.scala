package com.wavesplatform.dex

import org.scalatest.concurrent.PatienceConfiguration.Timeout

import scala.concurrent.duration.Duration

object Implicits {

  implicit def durationToScalatestTimeout(d: Duration): Timeout = Timeout(d)

}
