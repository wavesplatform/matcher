package com.wavesplatform.dex.it.test

import java.time.{LocalDateTime, ZoneId}

import com.wavesplatform.dex.it.api.BaseContainersKit
import mouse.any._
import org.scalatest._

import scala.util.{Failure, Success}

trait InformativeTestStart extends TestSuite { self: BaseContainersKit =>

  @volatile private var preventLogs = false

  override protected def runTest(testName: String, args: Args): Status = {
    if (shouldWrite(success = true)) print(s"STARTED: $testName")
    super.runTest(testName, args).unsafeTap {
      _.whenCompleted {
        case Success(success) => if (shouldWrite(success)) print(s"${if (success) "SUCCEEDED" else "FAILED"}: $testName")
        case Failure(e) => if (shouldWrite(success = false)) print(s"FAILED WITH ${e.getClass.getSimpleName}: $testName")
      }
    }
  }

  // Single quotes to emphasize the text in IDE
  private def print(text: String): Unit = writeGlobalLog(s"'---------- [${LocalDateTime.now(ZoneId.of("UTC"))}] $text ----------'")

  private def shouldWrite(success: Boolean): Boolean =
    this match {
      case _: CancelAfterFailure | _: NoStackTraceCancelAfterFailure =>
        if (preventLogs) false
        else if (success) true
        else {
          preventLogs = true
          true
        }
      case _ => true
    }

  protected def writeGlobalLog(x: String): Unit = {
    log.debug(x)
    knownContainers.get().foreach(_.printDebugMessage(x))
  }

}
