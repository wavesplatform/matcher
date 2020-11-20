package com.wavesplatform.dex.it.test

import java.time.{LocalDateTime, ZoneId}
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.dex.it.api.BaseContainersKit
import mouse.any._
import org.scalatest.{Args, CancelAfterFailure, Status, Suite}

import scala.util.{Failure, Success}

trait InformativeTestStart extends Suite { self: BaseContainersKit =>

  private val isFailed = new AtomicBoolean(false)

  override protected def runTest(testName: String, args: Args): Status = {

    def print(text: String): Unit = writeGlobalLog(s"---------- [${LocalDateTime.now(ZoneId.of("UTC"))}] $text ----------")

    print(s"STARTED: $testName")

    super.runTest(testName, args) unsafeTap {
      _.whenCompleted {
        case Success(r) => if (!isFailed.get) print(s"${if (r) "SUCCEEDED" else "FAILED"}: $testName")
        case Failure(e) =>
          val shouldWrite = this match {
            case _: CancelAfterFailure => isFailed.compareAndSet(false, true)
            case _ => true
          }
          if (shouldWrite) print(s"FAILED WITH ${e.getClass.getSimpleName}: $testName")
      }
    }
  }

  protected def writeGlobalLog(x: String): Unit = {
    log.debug(x)
    knownContainers.get().foreach(_.printDebugMessage(x))
  }

}
