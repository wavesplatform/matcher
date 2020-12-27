package com.wavesplatform.dex.actors.tx

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed._
import com.wavesplatform.dex.actors.events.{OrderEventsCoordinatorActor => OEC}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.Message
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

        val client = testKit.createTestProbe[OEC.Message]()
        val event = sampleEvent(client.ref)
        actor ! event
        client.expectMessageType[OEC.Command.ApplyConfirmed]
        broadcasted shouldBe Seq(event.tx)
      }

      "send a response to a client, if a transaction was confirmed before" in {
        val actor = defaultActor { _ =>
          Future.successful(CheckedBroadcastResult.Confirmed)
        }

        val client = testKit.createTestProbe[OEC.Message]()
        val event = sampleEvent(client.ref)
        actor ! event
        client.expectMessage(OEC.Command.ApplyConfirmed(event.tx))
      }

      "don't send messages to a client, if a transaction wasn't confirmed" - {
        def test(r: CheckedBroadcastResult): Unit = {
          val actor = defaultActor { _ =>
            Future.successful(r)
          }

          val client = testKit.createTestProbe[OEC.Message]()

          actor ! sampleEvent(client.ref)
          client.expectNoMessage()
        }

        "Unconfirmed" - {
          "true" in test(CheckedBroadcastResult.Unconfirmed(true))
          "false" in test(CheckedBroadcastResult.Unconfirmed(false))
        }

        "Failed" in test(CheckedBroadcastResult.Failed("test"))
      }

      "when an expired transaction" - {
        def test(check: (TestProbe[OEC.Message], Int) => Unit): Unit = {
          val attempts = new AtomicInteger(0)
          val actor = defaultActor { _ =>
            attempts.incrementAndGet()
            Future.successful(CheckedBroadcastResult.Failed("expired"))
          }

          val client = testKit.createTestProbe[OEC.Message]()

          actor ! sampleEvent(client.ref, createdTs = System.currentTimeMillis() - 1.day.toMillis)
          manualTime.timePasses(5.millis)

          check(client, attempts.get())
        }

        "doesn't broadcast" in test { (_, attempts) =>
          attempts shouldBe 0
        }

        "replies to a client" in test { (client, _) =>
          client.expectMessageType[OEC.Command.ApplyConfirmed]
        }
      }
    }

    "broadcast until" - {
      val notConfirmed: Vector[Future[CheckedBroadcastResult]] = Vector(
        Future.failed(new RuntimeException("exception") with NoStackTrace),
        Future.successful(CheckedBroadcastResult.Unconfirmed(false)),
        Future.successful(CheckedBroadcastResult.Unconfirmed(true)),
        Future.successful(CheckedBroadcastResult.Failed("failed"))
      )

      val settings = ExchangeTransactionBroadcastActor.Settings(
        interval = 20.millis,
        maxPendingTime = 200.millis
      )

      def mkActor(broadcast: => Future[CheckedBroadcastResult]): ActorRef[Message] = testKit.spawn(
        ExchangeTransactionBroadcastActor(
          settings = settings,
          blockchain = { _ => broadcast },
          time = new TestTime()
        )
      )

      "confirmed" in {
        val maxAttempts = 5
        val actualAttempts = new AtomicInteger(0)
        val actor = mkActor {
          if (actualAttempts.incrementAndGet() == maxAttempts) Future.successful(CheckedBroadcastResult.Confirmed)
          else notConfirmed(ThreadLocalRandom.current().nextInt(notConfirmed.size))
        }

        val client = testKit.createTestProbe[OEC.Message]()

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

      "timed out" in {
        val attempts = new AtomicInteger(0)
        val actor = mkActor {
          attempts.incrementAndGet()
          notConfirmed(ThreadLocalRandom.current().nextInt(notConfirmed.size))
        }

        val client = testKit.createTestProbe[OEC.Message]()

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
              notConfirmed(ThreadLocalRandom.current().nextInt(notConfirmed.size))
            },
            time = testTime
          )
        )

        val client = testKit.createTestProbe[OEC.Message]()

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
    testKit.spawn(
      ExchangeTransactionBroadcastActor(
        settings = ExchangeTransactionBroadcastActor.Settings(
          interval = 1.minute,
          maxPendingTime = 5.minute
        ),
        blockchain = broadcast(_),
        time = new TestTime()
      )
    )

  private def sampleEvent(
    clientRef: ActorRef[OEC.Message],
    createdTs: Long = System.currentTimeMillis(),
    time: Time = new TestTime()
  ): ExchangeTransactionBroadcastActor.Command.Broadcast = {
    val now = time.getTimestamp()
    val expiration = now + 1.day.toMillis
    ExchangeTransactionBroadcastActor.Command.Broadcast(
      clientRef,
      ExchangeTransactionV2
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
