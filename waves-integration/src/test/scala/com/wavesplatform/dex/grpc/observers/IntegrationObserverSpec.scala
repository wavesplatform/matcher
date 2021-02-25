package com.wavesplatform.dex.grpc.observers

import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class IntegrationObserverSpec extends WavesIntegrationSuiteBase with ScalaCheckDrivenPropertyChecks with NoShrink {

  "IntegrationObserver" - {
    "invariant: awaitNext == buffer.isEmpty" - {
      "awaitNext is false if the buffer isn't empty" - {
        "onNext" in {
          val t = mkDefault
          t.onNext(0)
          t.awaitNext.get() shouldBe false
        }

        "onNext, paired" in forAll(pairedGen) { calls =>
          val t = mkDefault
          t.onNext(0)
          calls.foreach { callRequestNext =>
            if (callRequestNext) t.requestNext()
            else t.onNext(0)
          }
          t.awaitNext.get() shouldBe false
        }
      }

      "awaitNext is true if the buffer is empty" - {
        "requestNext" in {
          val t = mkDefault
          t.requestNext()
          t.awaitNext.get() shouldBe true
        }

        "paired" in forAll(pairedGen) { calls =>
          val t = mkDefault
          calls.foreach { callRequestNext =>
            if (callRequestNext) t.requestNext()
            else t.onNext(0)
          }
          t.awaitNext.get() shouldBe true
        }
      }
    }
  }

  private def pairedGen: Gen[Vector[Boolean]] =
    Gen.containerOf[Vector, Int](Gen.choose(1, 3)).flatMap { pairsSeq =>
      pairsSeq
        .foldLeft(Vector.empty[Boolean]) { case (r, pairs) =>
          r ++ List.fill(pairs)(false) ++ List.fill(pairs)(true)
        }
    }

  private def mkDefault = new TestIntegrationObserver(Observer.empty(UncaughtExceptionReporter.default))

  private class TestIntegrationObserver(dest: Observer[Int]) extends IntegrationObserver[Boolean, Int](dest) {
    override def onError(t: Throwable): Unit = throw t
    override def onCompleted(): Unit = {}
  }

}
