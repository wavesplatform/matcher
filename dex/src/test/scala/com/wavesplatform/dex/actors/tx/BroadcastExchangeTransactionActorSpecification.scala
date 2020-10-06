package com.wavesplatform.dex.actors.tx

import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.MatcherSpec
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.{loadConfig, ExchangeTransactionBroadcastSettings}
import com.wavesplatform.dex.time.Time
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class BroadcastExchangeTransactionActorSpecification
    extends MatcherSpec("BroadcastExchangeTransactionActor")
    with MatcherSpecBase
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

  "BroadcastExchangeTransactionActor" should {
    "broadcast a transaction when receives it" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor = defaultActor(
        time,
        confirmed = _ => getConfirmation(false),
        broadcast = tx => {
          broadcasted = List(tx)
          Future.successful(true)
        }
      )

      val event = sampleEvent()
      actor ! event
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "broadcast a transaction in a next period if it wasn't confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor = defaultActor(
        time,
        confirmed = _ => getConfirmation(false),
        broadcast = tx => {
          broadcasted = List(tx)
          Future.successful(true)
        }
      )

      val event = sampleEvent()
      actor ! event
      broadcasted = Seq.empty

      // Will be re-sent on second call
      actor ! BroadcastExchangeTransactionActor.CheckAndSend
      actor ! BroadcastExchangeTransactionActor.CheckAndSend
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "doesn't broadcast a transaction if it was confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor =
        defaultActor(
          time,
          confirmed = _ => getConfirmation(true),
          broadcast = tx => {
            broadcasted = List(tx)
            Future.successful(true)
          }
        )

      val event = sampleEvent()
      actor ! event
      broadcasted = Seq.empty

      actor ! BroadcastExchangeTransactionActor.CheckAndSend
      actor ! BroadcastExchangeTransactionActor.CheckAndSend
      eventually {
        broadcasted shouldBe empty
      }
    }

    "doesn't broadcast an expired transaction" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor =
        defaultActor(
          time,
          confirmed = _ => getConfirmation(true),
          broadcast = tx => {
            broadcasted = List(tx)
            Future.successful(true)
          }
        )

      val event = sampleEvent(500.millis)
      actor ! event
      broadcasted = Seq.empty

      actor ! BroadcastExchangeTransactionActor.CheckAndSend
      actor ! BroadcastExchangeTransactionActor.CheckAndSend

      eventually {
        broadcasted shouldBe empty
      }
    }

    "retries" when {
      "failed to confirm (retry checks)" in {
        val firstProcessed = new AtomicBoolean(false)
        var triedToConfirm = Seq.empty[ByteStr]
        val actor = defaultActor(
          time,
          confirmed = { txs =>
            triedToConfirm = txs
            if (!firstProcessed.get) Future.successful(txs.map(id => id -> false).toMap)
            else Future.failed(new RuntimeException("Can't do this"))
          },
          broadcast = _ => {
            firstProcessed.compareAndSet(false, true)
            Future.successful(true)
          }
        )

        val event = sampleEvent()
        actor ! event
        eventually {
          firstProcessed.get shouldBe true
        }

        actor ! BroadcastExchangeTransactionActor.CheckAndSend
        actor ! BroadcastExchangeTransactionActor.CheckAndSend
        eventually {
          triedToConfirm should not be empty
        }
        triedToConfirm = Seq.empty

        actor ! BroadcastExchangeTransactionActor.CheckAndSend
        eventually {
          triedToConfirm should not be empty
        }
      }

      "failed to broadcast (retry)" in {
        val firstProcessing = new AtomicBoolean(false)
        var triedToBroadcast = Seq.empty[ExchangeTransaction]
        val actor = defaultActor(
          time,
          confirmed = _ => getConfirmation(false),
          broadcast = { txs =>
            firstProcessing.compareAndSet(false, true)
            triedToBroadcast = List(txs)
            Future.failed(new RuntimeException("Can't do"))
          }
        )

        val event = sampleEvent()
        actor ! event
        eventually {
          firstProcessing.get() shouldBe true
          triedToBroadcast should not be empty
        }

        triedToBroadcast = Seq.empty
        actor ! BroadcastExchangeTransactionActor.CheckAndSend
        eventually {
          triedToBroadcast should not be empty
        }
      }
    }
  }

  private def defaultActor(
    time: Time,
    confirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
    broadcast: ExchangeTransaction => Future[Boolean]
  ): TestActorRef[BroadcastExchangeTransactionActor] = TestActorRef(
    new BroadcastExchangeTransactionActor(
      settings = ExchangeTransactionBroadcastSettings(
        broadcastUntilConfirmed = true,
        interval = 1.minute,
        maxPendingTime = 5.minute
      ),
      time = time,
      confirmed = confirmed,
      broadcast = broadcast
    )
  )

  private def sampleEvent(expiration: FiniteDuration = 1.day): ExchangeTransactionCreated = {
    val ts = time.getTimestamp()
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
