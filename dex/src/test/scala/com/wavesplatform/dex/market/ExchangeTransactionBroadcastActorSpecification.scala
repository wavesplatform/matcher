package com.wavesplatform.dex.market

import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, ExchangeTransactionV2, Order}
import com.wavesplatform.utils.Time
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class ExchangeTransactionBroadcastActorSpecification
    extends MatcherSpec("ExchangeTransactionBroadcastActor")
    with MatcherTestData
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender
    with Eventually {

  implicit override lazy val system: ActorSystem = ActorSystem(
    actorSystemName,
    loadConfig(ConfigFactory.empty())
  )

  private val pair = AssetPair(IssuedAsset(Array.emptyByteArray), Waves)

  private def getConfirmation(allConfirmed: Boolean): Future[Map[ByteStr, Boolean]] = Future.successful {
    Map.empty[ByteStr, Boolean].withDefaultValue(allConfirmed)
  }

  "ExchangeTransactionBroadcastActor" should {
    "broadcast a transaction when receives it" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      defaultActor(
        ntpTime,
        isConfirmed = _ => getConfirmation(false),
        broadcast = tx => {
          broadcasted = List(tx)
          Future.successful(true)
        }
      )

      val event = sampleEvent()
      system.eventStream.publish(event)
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "broadcast a transaction in a next period if it wasn't confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor = defaultActor(
        ntpTime,
        isConfirmed = _ => getConfirmation(false),
        broadcast = tx => {
          broadcasted = List(tx)
          Future.successful(true)
        }
      )

      val event = sampleEvent()
      system.eventStream.publish(event)
      broadcasted = Seq.empty

      // Will be re-sent on second call
      actor ! ExchangeTransactionBroadcastActor.Send
      actor ! ExchangeTransactionBroadcastActor.Send
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "doesn't broadcast a transaction if it was confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor =
        defaultActor(
          ntpTime,
          isConfirmed = _ => getConfirmation(true),
          broadcast = tx => {
            broadcasted = List(tx)
            Future.successful(true)
          }
        )

      val event = sampleEvent()
      system.eventStream.publish(event)
      broadcasted = Seq.empty

      actor ! ExchangeTransactionBroadcastActor.Send
      actor ! ExchangeTransactionBroadcastActor.Send
      eventually {
        broadcasted shouldBe empty
      }
    }

    "doesn't broadcast an expired transaction" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor =
        defaultActor(
          ntpTime,
          isConfirmed = _ => getConfirmation(true),
          broadcast = tx => {
            broadcasted = List(tx)
            Future.successful(true)
          }
        )

      val event = sampleEvent(500.millis)
      system.eventStream.publish(event)
      broadcasted = Seq.empty

      actor ! ExchangeTransactionBroadcastActor.Send
      actor ! ExchangeTransactionBroadcastActor.Send

      eventually {
        broadcasted shouldBe empty
      }
    }

    "re-broadcasts transactions" when {
      "failed to validate" in {
        val firstProcessing = new AtomicBoolean(false)
        var triedToBroadcast = Seq.empty[ExchangeTransaction]
        val actor = defaultActor(
          ntpTime,
          check = _ => throw new RuntimeException("Can't do"),
          isConfirmed = _ => false,
          broadcast = {
            firstProcessing.compareAndSet(false, true)
            triedToBroadcast = _
          }
        )

        val event = sampleEvent()
        system.eventStream.publish(event)
        eventually {
          firstProcessing.get() shouldBe true
        }
        triedToBroadcast shouldBe empty // Because couldn't check

        actor ! ExchangeTransactionBroadcastActor.Send
        actor ! ExchangeTransactionBroadcastActor.Send
        eventually {
          triedToBroadcast should not be empty
        }
      }

      "failed to broadcast" in {
        val firstProcessing = new AtomicBoolean(false)
        var triedToBroadcast = Seq.empty[ExchangeTransaction]
        val actor = defaultActor(
          ntpTime,
          isConfirmed = _ => false,
          broadcast = { txs =>
            firstProcessing.compareAndSet(false, true)
            triedToBroadcast = txs
            throw new RuntimeException("Can't do")
          }
        )

        val event = sampleEvent()
        system.eventStream.publish(event)
        eventually {
          firstProcessing.get() shouldBe true
        }

        triedToBroadcast = Seq.empty
        actor ! ExchangeTransactionBroadcastActor.Send
        actor ! ExchangeTransactionBroadcastActor.Send
        eventually {
          triedToBroadcast should not be empty
        }
      }
    }
  }

  private def defaultActor(time: Time,
                           isConfirmed: ExchangeTransaction => Boolean,
                           broadcast: Seq[ExchangeTransaction] => Unit): TestActorRef[ExchangeTransactionBroadcastActor] =
    defaultActor(time, _ => Right(Unit), isConfirmed, broadcast)

  private def defaultActor(time: Time,
                           check: ExchangeTransaction => Either[ValidationError, Unit],
                           isConfirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
                           broadcast: ExchangeTransaction => Future[Boolean]): TestActorRef[ExchangeTransactionBroadcastActor] = TestActorRef(
    new ExchangeTransactionBroadcastActor(
      settings = ExchangeTransactionBroadcastSettings(
        broadcastUntilConfirmed = true,
        interval = 1.minute,
        maxPendingTime = 5.minute
      ),
      time = time,
      isConfirmed,
      broadcast = broadcast
    )
  )

  private def sampleEvent(expiration: FiniteDuration = 1.day): ExchangeTransactionCreated = {
    val ts = ntpTime.getTimestamp()
    ExchangeTransactionCreated(
      ExchangeTransactionV2
        .create(
          buyOrder = Order.buy(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = ts,
            expiration = ts + expiration.toMillis,
            matcherFee = 100
          ),
          sellOrder = Order.sell(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = ts,
            expiration = ts + expiration.toMillis,
            matcherFee = 100
          ),
          amount = 100,
          price = 6000000L,
          buyMatcherFee = 0L,
          sellMatcherFee = 0L,
          fee = 300000L,
          timestamp = ts,
          proofs = Proofs.empty
        )
        .explicitGet()
    )
  }
}
