package com.wavesplatform.dex.actors.events

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.{testkit => classic}
import cats.syntax.either._
import cats.syntax.option._
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.Command.Broadcast
import com.wavesplatform.dex.collections.{FifoSet, PositiveMap}
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.{Events, LimitOrder}
import com.wavesplatform.dex.{error, MatcherSpecBase}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag

class OrderEventsCoordinatorActorSpec extends ScalaTestWithActorTestKit() with MatcherSpecBase with AnyFreeSpecLike with Matchers {

  implicit private val actorSystem = testKit.internalSystem
  implicit private val classicActorSystem = actorSystem.classicSystem

  private val defaultSettings = OrderEventsCoordinatorActor.Settings(1000)
  private val assetPair = AssetPair(Waves, btc)

  "OrderEventsCoordinatorActor" - {
    "Process" - {
      "OrderAdded - passes" in {
        val event = Events.OrderAdded(
          LimitOrder(OrderV1(
            sender = privateKey("test"),
            matcher = PublicKey("matcher".getBytes("utf-8")),
            pair = assetPair,
            orderType = OrderType.BUY,
            price = 100000000L,
            amount = 100L,
            timestamp = nowTs,
            expiration = nowTs + 5.days.toMillis,
            matcherFee = matcherFee
          )),
          Events.OrderAddedReason.RequestExecuted,
          nowTs
        )
        passToAddressDirectoryTest(OrderEventsCoordinatorActor.Command.Process(event)) {
          _.expectMsg(AddressActor.Command.ApplyOrderBookAdded(event))
        }
      }

      "OrderCanceled - passes" in {
        val event = Events.OrderCanceled(
          LimitOrder(OrderV1(
            sender = privateKey("test"),
            matcher = PublicKey("matcher".getBytes("utf-8")),
            pair = assetPair,
            orderType = OrderType.BUY,
            price = 100000000L,
            amount = 100L,
            timestamp = nowTs,
            expiration = nowTs + 5.days.toMillis,
            matcherFee = matcherFee
          )),
          Events.OrderCanceledReason.RequestExecuted,
          nowTs
        )
        passToAddressDirectoryTest(
          OrderEventsCoordinatorActor.Command.Process(event),
          AddressActor.Command.ApplyOrderBookCanceled(event)
        )
      }

      "OrderExecuted" - {
        val expiration = nowTs + 10.days.toMillis
        "if is able to create a tx" - {
          val counter = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L))
          val submitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L))
          val event = Events.OrderExecuted(LimitOrder(submitted), LimitOrder(counter), nowTs, 300000L, 300000L)
          val tx = ExchangeTransactionV2
            .create(
              buyOrder = counter,
              sellOrder = submitted,
              amount = 100000,
              price = 80000L,
              buyMatcherFee = 2000L,
              sellMatcherFee = 1000L,
              fee = 300000L,
              timestamp = nowTs,
              proofs = Proofs.empty
            )
            .explicitGet()

          val addressDirectory = classic.TestProbe()
          val broadcastActor = TestProbe[ExchangeTransactionBroadcastActor.Command]()
          val dbWriter = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            dbWriter.ref,
            broadcastActor.ref,
            _ => tx.asRight
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.Process(event)

          "broadcasts it" in {
            val actual = broadcastActor.expectMessageType[ExchangeTransactionBroadcastActor.Command.Broadcast]
            val spendings = Map[Address, PositiveMap[Asset, Long]](
              counter.sender.toAddress -> PositiveMap(event.counterExecutedSpending),
              submitted.sender.toAddress -> PositiveMap(event.submittedExecutedSpending)
            )

            implicit val broadcastDiff: Derived[Diff[Broadcast]] = Derived(
              Diff
                .gen[Broadcast]
                .ignore[Broadcast, ActorRef[ExchangeTransactionBroadcastActor.Observed]](_.clientRef)
            )

            actual should matchTo(Broadcast(TestProbe[ExchangeTransactionBroadcastActor.Observed]().ref, spendings, tx))
          }

          "saves it" in {
            val actual = dbWriter.expectMsgType[ExchangeTransactionCreated]
            actual should matchTo(ExchangeTransactionCreated(tx))
          }

          "passes an event with a tx" in {
            val actual = addressDirectory.expectMsgType[AddressActor.Command.ApplyOrderBookExecuted]
            actual should matchTo(AddressActor.Command.ApplyOrderBookExecuted(event, tx.some))
          }
        }

        "if is not able to create a tx - passes an event without a tx" in {
          val buyOrder = Order.buy(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = assetPair,
            amount = 100,
            price = 6000000L,
            timestamp = nowTs,
            expiration = expiration,
            matcherFee = 300000L
          )
          val sellOrder = Order.sell(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = assetPair,
            amount = 100,
            price = 6000000L,
            timestamp = nowTs,
            expiration = expiration,
            matcherFee = 300000L
          )
          val event = Events.OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), nowTs, 300000L, 300000L)
          passToAddressDirectoryTest(
            OrderEventsCoordinatorActor.Command.Process(event),
            AddressActor.Command.ApplyOrderBookExecuted(event, none)
          )
        }
      }
    }

    "ProcessError - passes" in {
      val orderId = ByteStr("order-id".getBytes(StandardCharsets.UTF_8))
      val event = Events.OrderCancelFailed(orderId, error.UnexpectedError)
      passToAddressDirectoryTest(OrderEventsCoordinatorActor.Command.ProcessError(event), event)
    }

    "ApplyNodeUpdates" - {

    }

    "ApplyObservedByBroadcaster" - {
      val counter = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L))
      val submitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L))
      val event = Events.OrderExecuted(LimitOrder(submitted), LimitOrder(counter), nowTs, 300000L, 300000L)
      val tx = ExchangeTransactionV2
        .create(
          buyOrder = counter,
          sellOrder = submitted,
          amount = 100000,
          price = 80000L,
          buyMatcherFee = 2000L,
          sellMatcherFee = 1000L,
          fee = 300000L,
          timestamp = nowTs,
          proofs = Proofs.empty
        )
        .explicitGet()
      val spendings = Map[Address, PositiveMap[Asset, Long]](
        counter.sender.toAddress -> PositiveMap(event.counterExecutedSpending),
        submitted.sender.toAddress -> PositiveMap(event.submittedExecutedSpending)
      )

      "if a transaction is already observed - ignores it" in {
        val addressDirectory = classic.TestProbe()
        val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
          addressDirectory.ref,
          classic.TestProbe().ref,
          TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
          _ => ValidationError.GenericError("test").asLeft,
          FifoSet.limited[ExchangeTransaction.Id](10).append(tx.id())._1
        ))
        oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(tx, Map.empty)
        addressDirectory.expectNoMessage()
      }

      "otherwise" - {
        "sends a message to both actors" in {
          val addressDirectory = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            classic.TestProbe().ref,
            TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
            _ => ValidationError.GenericError("test").asLeft
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(tx, spendings)

          val actual1 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual1 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            counter.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              tx.id() -> PositiveMap(event.counterExecutedSpending)
            ))
          ))

          val actual2 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual2 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            submitted.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              tx.id() -> PositiveMap(event.submittedExecutedSpending)
            ))
          ))
        }

        "adds it to observed" in {
          val addressDirectory = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            classic.TestProbe().ref,
            TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
            _ => ValidationError.GenericError("test").asLeft
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(tx, spendings)
          (1 to 2).foreach(_ => addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]) // Ignore messages

          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(tx, spendings)
          addressDirectory.expectNoMessage()
        }
      }
    }
  }

  private def passToAddressDirectoryTest[T: ClassTag: Diff](command: OrderEventsCoordinatorActor.Command, expected: T): Unit =
    passToAddressDirectoryTest(command) { ad =>
      val actual = ad.expectMsgType[T]
      actual should matchTo(expected)
    }

  private def passToAddressDirectoryTest(command: OrderEventsCoordinatorActor.Command)(f: classic.TestProbe => Unit): Unit = {
    val addressDirectory = classic.TestProbe()
    val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
      defaultSettings,
      addressDirectory.ref,
      classic.TestProbe().ref,
      TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
      _ => ValidationError.GenericError("test").asLeft
    ))

    oecRef ! command
    f(addressDirectory)
  }

}
