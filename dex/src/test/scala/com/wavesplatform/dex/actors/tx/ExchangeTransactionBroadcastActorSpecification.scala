package com.wavesplatform.dex.actors.tx

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed._
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Message, Observed, Settings}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.CheckedBroadcastResult
import com.wavesplatform.dex.time.{TestTime, Time}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.control.NoStackTrace

class ExchangeTransactionBroadcastActorSpecification
    extends ScalaTestWithActorTestKit(ManualTime.config)
    with AnyFreeSpecLike
    with Matchers
    with BeforeAndAfterAll
    with PathMockFactory
    with Eventually {

  private val manualTime: ManualTime = ManualTime()

  private val pair = AssetPair(IssuedAsset(Array.emptyByteArray), Waves)

  "ExchangeTransactionBroadcastActor" - {
    "Command.Broadcast" - {
      "broadcast a transaction when receives it" in {
        var broadcasted = Seq.empty[ExchangeTransaction]
        val actor = defaultActor { tx =>
          broadcasted = List(tx)
          Future.successful(CheckedBroadcastResult.Confirmed)
        }

        val client = testKit.createTestProbe[Observed]()
        val event = sampleEvent(client.ref)
        actor ! event
        client.expectMessageType[Observed]
        broadcasted shouldBe Seq(event.tx)
      }

      "send a response to a client, if a transaction" - {
        def test(result: CheckedBroadcastResult): Unit = {
          val actor = defaultActor { _ =>
            Future.successful(result)
          }

          val client = testKit.createTestProbe[Observed]()
          val event = sampleEvent(client.ref)
          actor ! event
          client.expectMessage(Observed(event.tx, Map.empty))
        }

        "confirmed before" in test(CheckedBroadcastResult.Confirmed)
        "failed and we can't retry" in test(CheckedBroadcastResult.Failed("error", canRetry = false))

        "failed and this was the last attempt" in {
          val result = CheckedBroadcastResult.Failed("error", canRetry = true)

          val settings = Settings(100.millis, 500.millis)
          val actor = defaultActorWithSettings(settings) { _ =>
            Future.successful(result)
          }

          val client = testKit.createTestProbe[Observed]()
          val event = sampleEvent(client.ref)

          actor ! event
          (1 to 5).foreach(_ => manualTime.timePasses(settings.interval))

          client.expectMessage(Observed(event.tx, Map.empty))
        }

        "expired and this was the last attempt" in {
          val result = CheckedBroadcastResult.Failed("error", canRetry = true)

          val settings = Settings(100.millis, 500.millis)

          val ntpTime = new TestTime()
          val actor = testKit.spawn(
            ExchangeTransactionBroadcastActor(
              settings = settings,
              blockchain = { _ => Future.successful(result) },
              time = ntpTime
            )
          )

          val client = testKit.createTestProbe[Observed]()
          val event = sampleEvent(client.ref)

          actor ! event
          (1 to 3).foreach(_ => manualTime.timePasses(settings.interval))
          ntpTime.advance(10.days)
          manualTime.timePasses(settings.interval)

          client.expectMessage(Observed(event.tx, Map.empty))
        }
      }

      "don't send a response to a client" - {
        def test(r: CheckedBroadcastResult): Unit = {
          val actor = defaultActor { _ =>
            Future.successful(r)
          }

          val client = testKit.createTestProbe[Observed]()

          actor ! sampleEvent(client.ref)
          client.expectNoMessage()
        }

        // Because they will be received from the stream
        "if a transaction is Unconfirmed and isNew=" - {
          "true" in test(CheckedBroadcastResult.Unconfirmed(true))
          "false" in test(CheckedBroadcastResult.Unconfirmed(false))
        }

        // Because it hasn't yet appeared in UTX
        "if a transaction is Failed and we can retry" in test(CheckedBroadcastResult.Failed("test", canRetry = true))
      }

      "when an expired transaction" - {
        def test(check: (TestProbe[Observed], Int) => Unit): Unit = {
          val attempts = new AtomicInteger(0)
          val actor = defaultActor { _ =>
            attempts.incrementAndGet()
            Future.successful(CheckedBroadcastResult.Failed("expired", canRetry = true)) // We don't reach this
          }

          val client = testKit.createTestProbe[Observed]()

          actor ! sampleEvent(client.ref, createdTs = System.currentTimeMillis() - 1.day.toMillis)
          manualTime.timePasses(5.millis)

          check(client, attempts.get())
        }

        "doesn't broadcast" in test((_, attempts) => attempts shouldBe 0)
        "replies to a client" in test((client, _) => client.expectMessageType[Observed])
      }
    }

    "broadcast until" - {
      val canRetry: Vector[Future[CheckedBroadcastResult]] = Vector(
        Future.failed(new RuntimeException("exception") with NoStackTrace),
        Future.successful(CheckedBroadcastResult.Unconfirmed(false)),
        Future.successful(CheckedBroadcastResult.Unconfirmed(true)),
        Future.successful(CheckedBroadcastResult.Failed("failed", canRetry = true))
      )

      val settings = Settings(
        interval = 20.millis,
        maxPendingTime = 200.millis
      )

      def mkActor(broadcast: => Future[CheckedBroadcastResult]): ActorRef[Message] = defaultActorWithSettings(settings)(_ => broadcast)

      def stopTest(lastResult: CheckedBroadcastResult): Unit = {
        val maxAttempts = 5
        val actualAttempts = new AtomicInteger(0)
        val actor = mkActor {
          if (actualAttempts.incrementAndGet() == maxAttempts) Future.successful(lastResult)
          else canRetry(ThreadLocalRandom.current().nextInt(canRetry.size))
        }

        val client = testKit.createTestProbe[Observed]()

        actor ! sampleEvent(client.ref)
        (1 to maxAttempts).foreach(_ => manualTime.timePasses(21.millis)) // Once it was sent immediately

        eventually {
          actualAttempts.get() shouldBe maxAttempts
        }

        withClue("no more retries: ") {
          manualTime.timePasses(21.millis)
          actualAttempts.get() shouldBe maxAttempts
        }
      }

      "confirmed" in stopTest(CheckedBroadcastResult.Confirmed)
      "failed and we can't retry" in stopTest(CheckedBroadcastResult.Failed("test", canRetry = false))

      "timed out" in {
        val attempts = new AtomicInteger(0)
        val actor = mkActor {
          attempts.incrementAndGet()
          canRetry(ThreadLocalRandom.current().nextInt(canRetry.size))
        }

        val client = testKit.createTestProbe[Observed]()

        actor ! sampleEvent(client.ref)
        (1 to 12).foreach(_ => manualTime.timePasses(21.millis))

        eventually {
          attempts.get() shouldBe (settings.maxPendingTime / settings.interval).toInt
        }
      }

      "expired" in {
        val testTime = new TestTime(System.currentTimeMillis())
        val attempts = new AtomicInteger(0)
        val actor = testKit.spawn(
          ExchangeTransactionBroadcastActor(
            settings = settings,
            blockchain = { _ =>
              attempts.incrementAndGet()
              canRetry(ThreadLocalRandom.current().nextInt(canRetry.size))
            },
            time = testTime
          )
        )

        val client = testKit.createTestProbe[Observed]()

        actor ! sampleEvent(
          client.ref,
          createdTs = testTime.getTimestamp() - ExchangeTransactionBroadcastActor.ExchangeTransactionExpirationMillis + 10
        )

        manualTime.timePasses(10.millis)
        attempts.get() shouldBe 1
        testTime.advance(10.millis)

        (1 to 10).foreach { _ =>
          manualTime.timePasses(21.millis)
          testTime.advance(21.millis)
        }

        attempts.get() shouldBe 1
      }
    }
  }

  private def defaultActor(broadcast: ExchangeTransaction => Future[CheckedBroadcastResult]): ActorRef[Message] =
    defaultActorWithSettings(Settings(
      interval = 1.minute,
      maxPendingTime = 5.minute
    ))(broadcast)

  private def defaultActorWithSettings(settings: Settings)(broadcast: ExchangeTransaction => Future[CheckedBroadcastResult]): ActorRef[Message] =
    testKit.spawn(
      ExchangeTransactionBroadcastActor(
        settings = settings,
        blockchain = broadcast(_),
        time = new TestTime()
      )
    )

  private def sampleEvent(
    clientRef: ActorRef[Observed],
    createdTs: Long = System.currentTimeMillis(),
    time: Time = new TestTime()
  ): ExchangeTransactionBroadcastActor.Command.Broadcast = {
    val now = time.getTimestamp()
    val expiration = now + 1.day.toMillis
    ExchangeTransactionBroadcastActor.Command.Broadcast(
      clientRef = clientRef,
      addressSpendings = Map.empty,
      tx = ExchangeTransactionV2
        .create(
          buyOrder = Order.buy(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = now,
            expiration = expiration,
            matcherFee = 100
          ),
          sellOrder = Order.sell(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = now,
            expiration = expiration,
            matcherFee = 100
          ),
          amount = 100,
          price = 6000000L,
          buyMatcherFee = 0L,
          sellMatcherFee = 0L,
          fee = 300000L,
          timestamp = createdTs,
          proofs = Proofs.empty
        )
        .explicitGet()
    )
  }

}
