package com.wavesplatform.dex

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.dex.AddressActor.Command.CancelNotEnoughCoinsOrders
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances}
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder, OrderBook}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.time.NTPTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Future
import scala.concurrent.duration._

class ActorsWebSocketInteractionsSpecification
    extends TestKit(ActorSystem("ActorsWebSocketInteractionsSpecification"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with NTPTime
    with MatcherSpecBase {

  private implicit val efc: ErrorFormatterContext = (_: Asset) => 8

  private def webSocketTest(
      f: (
          ActorRef, // address actor
          TestProbe, // test probe
          KeyPair, // owner's key pair
          AcceptedOrder => Unit, // add order
          (AcceptedOrder, Boolean) => Unit, // cancel
          (AcceptedOrder, LimitOrder) => OrderExecuted, // execute
          (Boolean, Seq[(Asset, Long)]) => Unit, // update spendable balances
          Seq[(Asset, WsBalances)] => Unit // expect balances diff by web socket
      ) => Unit
  ): Unit = {

    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = KeyPair("test".getBytes)

    def allAssetsSpendableBalance: Address => Future[Map[Asset, Long]] = { _ =>
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance) }
    }

    val spendableBalancesActor =
      system.actorOf(
        Props(new SpendableBalancesActor(allAssetsSpendableBalance)(scala.concurrent.ExecutionContext.Implicits.global))
      )

    val addressActor =
      system.actorOf(
        Props(
          new AddressActor(
            address,
            x => Future.successful { currentPortfolio.get().spendableBalanceOf(x) },
            ntpTime,
            EmptyOrderDB,
            _ => Future.successful(false),
            event => {
              eventsProbe.ref ! event
              Future.successful { Some(QueueEventWithMeta(0, 0, event)) }
            },
            _ => OrderBook.AggregatedSnapshot(),
            false,
            spendableBalancesActor
          )
        )
      )

    def addOrder(ao: AcceptedOrder): Unit = {
      addressActor ! AddressActor.Command.PlaceOrder(ao.order, ao.isMarket)
      ao.fold { lo =>
        eventsProbe.expectMsg(QueueEvent.Placed(lo))
        addressActor ! OrderAdded(lo, System.currentTimeMillis)
      } { mo =>
        eventsProbe.expectMsg(QueueEvent.PlacedMarket(mo))
      }
    }

    def cancelOrder(ao: AcceptedOrder, isSystemCancel: Boolean): Unit = {
      if (ao.isLimit) {
        addressActor ! AddressActor.Command.CancelOrder(ao.order.id())
        eventsProbe.expectMsg(QueueEvent.Canceled(ao.order.assetPair, ao.order.id()))
      }
      addressActor ! OrderCanceled(ao, isSystemCancel, System.currentTimeMillis)
    }

    def executeOrder(s: AcceptedOrder, c: LimitOrder): OrderExecuted = {
      val oe = OrderExecuted(s, c, System.currentTimeMillis, s.matcherFee, c.matcherFee)
      addressActor ! oe
      oe
    }

    def expectWebSocketBalance(expected: Seq[(Asset, WsBalances)]): Unit = {
      eventsProbe.expectMsgAnyClassOf(1.second, classOf[WsAddressState]).balances should matchTo(expected.toMap)
    }

    f(
      addressActor,
      eventsProbe,
      address,
      addOrder,
      cancelOrder,
      executeOrder,
      (notify, changes) => {

        val cm               = changes.toMap
        val updatedPortfolio = Portfolio(cm.getOrElse(Waves, 0), LeaseBalance.empty, cm.collect { case (a: IssuedAsset, b) => a -> b })
        val prevPortfolio    = currentPortfolio.getAndSet(updatedPortfolio)

        if (notify) {

          val spendableBalanceChanges: Map[Asset, Long] =
            prevPortfolio
              .changedAssetIds(updatedPortfolio)
              .map(asset => asset -> updatedPortfolio.spendableBalanceOf(asset))
              .toMap
              .withDefaultValue(0)

          addressActor ! CancelNotEnoughCoinsOrders(spendableBalanceChanges)
          spendableBalancesActor ! SpendableBalancesActor.Command.UpdateDiff(Map(address.toAddress -> spendableBalanceChanges))
        }
      },
      expectWebSocketBalance
    )

    addressActor ! PoisonPill
  }

  "Actors web socket interaction" should {
    "correctly process web socket requests" when {

      "sender places order and then cancel it" in webSocketTest { (aa, ep, address, addOrder, cancel, _, updateBalances, expectWsBalance) =>
        updateBalances(false, Seq(Waves -> 100.waves, usd -> 300.usd))

        aa.tell(AddressActor.AddWsSubscription, ep.ref)
        expectWsBalance(Seq(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0)))

        val order = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = address))

        addOrder(order)
        expectWsBalance(Seq(usd -> WsBalances(285.usd, 15.usd)))

        cancel(order, false)
        expectWsBalance(Seq(usd -> WsBalances(300.usd, 0)))
      }

      "sender places order, receives updates by third asset, partly fills order and then cancels remaining" in webSocketTest {
        (aa, ep, address, place, cancel, executeOrder, updateBalances, expectWsBalance) =>
          withClue(s"Sender has 100 Waves and 300 USD, requests snapshot\n") {

            updateBalances(false, Seq(Waves -> 100.waves, usd -> 300.usd))
            aa.tell(AddressActor.AddWsSubscription, ep.ref)

            expectWsBalance(
              Seq(
                Waves -> WsBalances(100.waves, 0),
                usd   -> WsBalances(300.usd, 0)
              )
            )
          }

          val buyOrder  = LimitOrder(createOrder(wavesUsdPair, BUY, 10.waves, 3.0, sender = address))
          val sellOrder = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0))

          withClue(s"Sender places order BUY 10 Waves\n") {
            place(buyOrder)
            expectWsBalance(Seq(usd -> WsBalances(270.usd, 30.usd)))
          }

          withClue(s"Sender received some ETH and this transfer transaction was forged\n") {
            updateBalances(true, Seq(Waves -> 100.waves, usd -> 300.usd, eth -> 4.eth))
            expectWsBalance(Seq(eth        -> WsBalances(4.eth, 0)))
          }

          val oe = executeOrder(sellOrder, buyOrder)

          withClue(s"Sender's order was partly filled by SELL 5 Waves. Balance changes are not atomic\n") {
            expectWsBalance(Seq(usd -> WsBalances(270.usd, 15.usd))) // first we send decreased balances

            updateBalances(true, Seq(Waves -> 105.waves, usd -> 285.usd)) // then we receive balance changes from blockchain

            expectWsBalance(
              Seq(
                Waves -> WsBalances(105.waves, 0),
                usd   -> WsBalances(270.usd, 15.usd)
              )
            )
          }

          withClue("Cancelling remaining of the counter order\n") {
            cancel(oe.counterRemaining, false)
            expectWsBalance(Seq(usd -> WsBalances(285.usd, 0)))
          }
      }

      "sender places market order in nonempty order book, fee in ETH" in webSocketTest {
        (aa, ep, address, addOrder, cancel, executeOrder, updateBalances, expectWsBalance) =>
          def matchOrders(submittedMarket: MarketOrder, counterAmount: Long, expectedChanges: Map[Asset, WsBalances]): MarketOrder = {

            val counter = LimitOrder(createOrder(wavesUsdPair, SELL, counterAmount, 3.00))
            val oe      = executeOrder(submittedMarket, counter)

            expectWsBalance(expectedChanges.toSeq)
            oe.submittedMarketRemaining(submittedMarket)
          }

          val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)
          updateBalances(false, tradableBalance.toSeq)

          aa.tell(AddressActor.AddWsSubscription, ep.ref)
          expectWsBalance(Seq(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0), eth -> WsBalances(2.eth, 0)))

          var mo = MarketOrder(createOrder(wavesUsdPair, BUY, 50.waves, 3.0, 0.00001703.eth, feeAsset = eth, sender = address), tradableBalance)
          addOrder(mo)

          withClue(s"First counter with 10 Waves, reserves: 150 - 30 USD, 0.00001703 - (0.00001703 / 5 = 0.00000340) ETH\n") {
            mo = matchOrders(mo, 10.waves, Map(usd -> WsBalances(150.usd, 120.usd), eth -> WsBalances(1.99998297.eth, 0.00001363.eth)))
          }

          withClue(s"Second counter with 15 Waves, reserves: 120 - 45 USD, 0.00001363 - (0.00001703 / (50 / 15)) = 0.00000510) ETH\n") {
            mo = matchOrders(mo, 15.waves, Map(usd -> WsBalances(150.usd, 75.usd), eth -> WsBalances(1.99998297.eth, 0.00000853.eth)))
          }

          withClue(s"Third counter with 5 Waves, reserves: 75 - 15 USD, 0.00000853 - (0.00001703 / 10) = 0.00000170) ETH\n") {
            mo = matchOrders(mo, 5.waves, Map(usd -> WsBalances(150.usd, 60.usd), eth -> WsBalances(1.99998297.eth, 0.00000683.eth)))
          }

          withClue(s"System cancel of market order remaining") {
            cancel(mo, true)
            expectWsBalance(Seq(usd -> WsBalances(210.usd, 0), eth -> WsBalances(1.9999898.eth, 0)))
          }

          withClue(s"First transaction forged\n") {
            updateBalances(true, Seq(Waves -> 110.waves, usd                -> 270.usd, eth                -> 1.99998637.eth))
            expectWsBalance(Seq(Waves      -> WsBalances(110.waves, 0), usd -> WsBalances(270.usd, 0), eth -> WsBalances(1.99998637.eth, 0)))
          }

          withClue(s"Second transaction forged\n") {
            updateBalances(true, Seq(Waves -> 125.waves, usd                -> 225.usd, eth                -> 1.99997784.eth))
            expectWsBalance(Seq(Waves      -> WsBalances(125.waves, 0), usd -> WsBalances(225.usd, 0), eth -> WsBalances(1.99997784.eth, 0)))
          }

          withClue(s"Third transaction forged\n") {
            updateBalances(true, Seq(Waves -> 130.waves, usd                -> 210.usd, eth                -> 1.99998297.eth))
            expectWsBalance(Seq(Waves      -> WsBalances(130.waves, 0), usd -> WsBalances(210.usd, 0), eth -> WsBalances(1.99998297.eth, 0)))
          }
      }
    }
  }
}
