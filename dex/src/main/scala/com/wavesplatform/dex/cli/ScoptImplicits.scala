package com.wavesplatform.dex.cli

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ScoptImplicits {
  implicit val finiteDurationScoptRead: scopt.Read[FiniteDuration] = scopt.Read.reads { x =>
    Duration(x) match {
      case x: FiniteDuration => x
      case _                 => throw new IllegalArgumentException("'" + x + "' is not a finite duration.")
    }
  }
}
