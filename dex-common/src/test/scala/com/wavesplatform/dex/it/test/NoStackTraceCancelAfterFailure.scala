package com.wavesplatform.dex.it.test

import org.scalatest.{CancelAfterFailure, Canceled, Outcome, TestSuite}

trait NoStackTraceCancelAfterFailure extends CancelAfterFailure { this: TestSuite =>

  abstract override def withFixture(test: NoArgTest): Outcome = super.withFixture(test) match {
    case x: Canceled =>
      x.exception.setStackTrace(Array.empty)
      x
    case x => x
  }

}
