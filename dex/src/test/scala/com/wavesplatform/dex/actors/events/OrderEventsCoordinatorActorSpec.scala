package com.wavesplatform.dex.actors.events

import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.{testkit => classic}
import cats.data.NonEmptyList
import cats.syntax.option._
import com.softwaremill.diffx.{Derived, Diff}
import com.wavesplatform.dex.actors.address.AddressActor.Command.ObservedTxData
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
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionResult, ExchangeTransactionV2}
import com.wavesplatform.dex.grpc.integration.clients.domain.{AddressBalanceUpdates, TransactionWithChanges, WavesNodeUpdates}
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.{Events, LimitOrder}
import com.wavesplatform.dex.{error, MatcherSpecBase}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.{SignedTransaction, Transaction}
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

  private val validCounter = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L))
  private val validSubmitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L))
  private val validEvent = Events.OrderExecuted(LimitOrder(validSubmitted), LimitOrder(validCounter), nowTs, 2000L, 1000L, 0L)

  private val validTx = ExchangeTransactionV2
    .create(
      buyOrder = validCounter,
      sellOrder = validSubmitted,
      amount = 100000,
      price = 80000L,
      buyMatcherFee = 2000L,
      sellMatcherFee = 1000L,
      fee = 300000L,
      timestamp = nowTs,
      proofs = Proofs.empty
    ).transaction

  private val validTxSpendings = Map[Address, PositiveMap[Asset, Long]](
    validCounter.sender.toAddress -> PositiveMap(validEvent.counterExecutedSpending),
    validSubmitted.sender.toAddress -> PositiveMap(validEvent.submittedExecutedSpending)
  )

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
        passToAddressDirectoryTest(OrderEventsCoordinatorActor.Command.Process(NonEmptyList.one(event))) {
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
          OrderEventsCoordinatorActor.Command.Process(NonEmptyList.one(event)),
          AddressActor.Command.ApplyOrderBookCanceled(event)
        )
      }

      "OrderExecuted" - {
        val expiration = nowTs + 10.days.toMillis
        "if is able to create a tx" - {
          val addressDirectory = classic.TestProbe()
          val broadcastActor = TestProbe[ExchangeTransactionBroadcastActor.Command]()
          val dbWriter = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            dbWriter.ref,
            broadcastActor.ref,
            _ => ExchangeTransactionResult(validTx)
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.Process(NonEmptyList.one(validEvent))

          "broadcasts it" in {
            val actual = broadcastActor.expectMessageType[ExchangeTransactionBroadcastActor.Command.Broadcast]
            val spendings = Map[Address, PositiveMap[Asset, Long]](
              validCounter.sender.toAddress -> PositiveMap(validEvent.counterExecutedSpending),
              validSubmitted.sender.toAddress -> PositiveMap(validEvent.submittedExecutedSpending)
            )

            implicit val broadcastDiff: Derived[Diff[Broadcast]] = Derived(
              Diff
                .gen[Broadcast]
                .ignore[Broadcast, ActorRef[ExchangeTransactionBroadcastActor.Observed]](_.clientRef)
            )

            actual should matchTo(Broadcast(TestProbe[ExchangeTransactionBroadcastActor.Observed]().ref, spendings, validTx))
          }

          "saves it" in {
            val actual = dbWriter.expectMsgType[ExchangeTransactionCreated]
            actual should matchTo(ExchangeTransactionCreated(validTx))
          }

          "passes an event with a tx" in {
            val actual = addressDirectory.expectMsgType[AddressActor.Command.ApplyOrderBookExecuted]
            actual should matchTo(AddressActor.Command.ApplyOrderBookExecuted(AddressActor.OrderBookExecutedEvent(
              validEvent,
              ExchangeTransactionResult(validTx)
            )))
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
          val event = Events.OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), nowTs, 300000L, 300000L, 0L)
          passToAddressDirectoryTest(
            OrderEventsCoordinatorActor.Command.Process(NonEmptyList.one(event)),
            AddressActor.Command.ApplyOrderBookExecuted(AddressActor.OrderBookExecutedEvent(
              event,
              ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
            ))
          )
        }
      }
    }

    "ProcessError - passes" in {
      val orderId = ByteStr("order-id".getBytes(StandardCharsets.UTF_8))
      val event = Events.OrderCancelFailed(orderId, error.UnexpectedError, None)
      passToAddressDirectoryTest(OrderEventsCoordinatorActor.Command.ProcessError(event), event)
    }

    "ApplyNodeUpdates" - {
      val validTxWithChanges = TransactionWithChanges(
        validTx.id().toPB,
        tx = SignedTransaction(
          transaction = validTx.toPB.transaction.map { tx =>
            Transaction(
              chainId = tx.chainId,
              senderPublicKey = tx.senderPublicKey,
              fee = tx.fee,
              timestamp = tx.timestamp,
              version = tx.version,
              data = com.wavesplatform.protobuf.transaction.Transaction.Data.Exchange(tx.getExchange)
            )
          }
        ),
        changes = StateUpdate()
      )

      "if a transaction is already observed - ignores it" in {
        val addressDirectory = classic.TestProbe()
        val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
          addressDirectory.ref,
          classic.TestProbe().ref,
          TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
          _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some),
          FifoSet.limited[ExchangeTransaction.Id](10).append(validTx.id())._1
        ))
        oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
          balanceUpdates = Map.empty,
          observedTxs = Map(validTx.id() -> validTxWithChanges)
        ))
        addressDirectory.expectNoMessage()
      }

      "if a transaction is not observed - adds it" in {
        val addressDirectory = classic.TestProbe()
        val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
          defaultSettings,
          addressDirectory.ref,
          classic.TestProbe().ref,
          TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
          _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
        ))
        oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
          balanceUpdates = Map.empty,
          observedTxs = Map(validTx.id() -> validTxWithChanges)
        ))
        (1 to 2).foreach(_ => addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]) // Ignore messages
        oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
          balanceUpdates = Map.empty,
          observedTxs = Map(validTx.id() -> validTxWithChanges)
        ))
        addressDirectory.expectNoMessage()
      }

      "sends" - {
        "MarkTxsObserved" in {
          val addressDirectory = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            classic.TestProbe().ref,
            TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
            _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
            balanceUpdates = Map.empty,
            observedTxs = Map(validTx.id() -> validTxWithChanges)
          ))

          val actual1 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual1 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validCounter.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              validTx.id() -> mkObservedTxData(PositiveMap(validEvent.counterExecutedSpending))
            ))
          ))

          val actual2 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual2 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validSubmitted.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              validTx.id() -> mkObservedTxData(PositiveMap(validEvent.submittedExecutedSpending))
            ))
          ))
        }

        "ChangeBalances" in {
          val addressDirectory = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            classic.TestProbe().ref,
            TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
            _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
          ))
          val addressBalanceUpdates = AddressBalanceUpdates(
            regular = Map(Waves -> 10L),
            outgoingLeasing = none,
            pessimisticCorrection = Map.empty
          )
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
            balanceUpdates = Map(validCounter.sender.toAddress -> addressBalanceUpdates),
            observedTxs = Map.empty
          ))

          val actual = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validCounter.sender.toAddress,
            AddressActor.Command.ChangeBalances(addressBalanceUpdates)
          ))
        }

        "ApplyBatch" in {
          val addressDirectory = classic.TestProbe()
          val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
            defaultSettings,
            addressDirectory.ref,
            classic.TestProbe().ref,
            TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
            _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
          ))
          val addressBalanceUpdates = AddressBalanceUpdates(
            regular = Map(Waves -> 10L),
            outgoingLeasing = none,
            pessimisticCorrection = Map.empty
          )
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyNodeUpdates(WavesNodeUpdates(
            balanceUpdates = Map(validCounter.sender.toAddress -> addressBalanceUpdates),
            observedTxs = Map(validTx.id() -> validTxWithChanges)
          ))

          val actual = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validCounter.sender.toAddress,
            AddressActor.Command.ApplyBatch(
              AddressActor.Command.MarkTxsObserved(Map(
                validTx.id() -> mkObservedTxData(PositiveMap(validEvent.counterExecutedSpending))
              )),
              AddressActor.Command.ChangeBalances(addressBalanceUpdates)
            )
          ))
        }
      }
    }

    "ApplyObservedByBroadcaster" - {
      "if a transaction is already observed - ignores it" in {
        val addressDirectory = classic.TestProbe()
        val oecRef = testKit.spawn(OrderEventsCoordinatorActor(
          addressDirectory.ref,
          classic.TestProbe().ref,
          TestProbe[ExchangeTransactionBroadcastActor.Command]().ref,
          _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some),
          FifoSet.limited[ExchangeTransaction.Id](10).append(validTx.id())._1
        ))
        oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(validTx, Map.empty)
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
            _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(validTx, validTxSpendings)

          val actual1 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual1 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validCounter.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              validTx.id() -> mkObservedTxData(PositiveMap(validEvent.counterExecutedSpending))
            ))
          ))

          val actual2 = addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]
          actual2 should matchTo(AddressDirectoryActor.Command.ForwardMessage(
            validSubmitted.sender.toAddress,
            AddressActor.Command.MarkTxsObserved(Map(
              validTx.id() -> mkObservedTxData(PositiveMap(validEvent.submittedExecutedSpending))
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
            _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
          ))
          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(validTx, validTxSpendings)
          (1 to 2).foreach(_ => addressDirectory.expectMsgType[AddressDirectoryActor.Command.ForwardMessage]) // Ignore messages

          oecRef ! OrderEventsCoordinatorActor.Command.ApplyObservedByBroadcaster(validTx, validTxSpendings)
          addressDirectory.expectNoMessage()
        }
      }
    }
  }

  private def mkObservedTxData(pessimisticChanges: PositiveMap[Asset, Long]): ObservedTxData =
    ObservedTxData(Seq(validCounter, validSubmitted), pessimisticChanges)

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
      _ => ExchangeTransactionResult(validTx, ValidationError.GenericError("test").some)
    ))

    oecRef ! command
    f(addressDirectory)
  }

}
