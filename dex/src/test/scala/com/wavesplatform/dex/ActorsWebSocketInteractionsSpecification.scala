package com.wavesplatform.dex

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances}
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder, OrderBookAggregatedSnapshot}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
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
    with MatcherSpecBase {

  private implicit val efc: ErrorFormatterContext = (_: Asset) => 8

  private def webSocketTest(
      f: (
          ActorRef, // address directory
          TestProbe, // test probe
          KeyPair, // owner's key pair
          => Unit, // subscribe
          AcceptedOrder => Unit, // add order
          (AcceptedOrder, Boolean) => Unit, // cancel
          (AcceptedOrder, LimitOrder) => OrderExecuted, // execute
          Map[Asset, Long] => Unit, // update spendable balances
          Map[Asset, WsBalances] => Unit // expect balances diff by web socket
      ) => Unit
  ): Unit = {

    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = KeyPair("test".getBytes)

    def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance).filterKeys(assets.contains) }
    }

    def allAssetsSpendableBalance: Address => Future[Map[Asset, Long]] = { _ =>
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance) }
    }

    lazy val addressDir = system.actorOf(Props(new AddressDirectory(EmptyOrderDB, createAddressActor, None)))

    lazy val spendableBalancesActor =
      system.actorOf(
        Props(new SpendableBalancesActor(spendableBalances, allAssetsSpendableBalance, addressDir))
      )

    def createAddressActor(address: Address, enableSchedules: Boolean): Props = {
      Props(
        new AddressActor(
          address,
          time,
          EmptyOrderDB,
          _ => Future.successful(false),
          event => {
            eventsProbe.ref ! event
            Future.successful { Some(QueueEventWithMeta(0, 0, event)) }
          },
          _ => OrderBookAggregatedSnapshot.empty,
          enableSchedules,
          spendableBalancesActor
        )
      )
    }

    def subscribe(): Unit = {
      addressDir.tell(AddressDirectory.Envelope(address, AddressActor.AddWsSubscription), eventsProbe.ref)
    }

    def addOrder(ao: AcceptedOrder): Unit = {
      addressDir ! AddressDirectory.Envelope(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      ao.fold { lo =>
        eventsProbe.expectMsg(QueueEvent.Placed(lo))
        addressDir ! OrderAdded(lo, System.currentTimeMillis)
      } { mo =>
        eventsProbe.expectMsg(QueueEvent.PlacedMarket(mo))
      }
    }

    def cancelOrder(ao: AcceptedOrder, isSystemCancel: Boolean): Unit = {
      if (ao.isLimit) {
        addressDir ! AddressDirectory.Envelope(address, AddressActor.Command.CancelOrder(ao.order.id()))
        eventsProbe.expectMsg(QueueEvent.Canceled(ao.order.assetPair, ao.order.id()))
      }
      addressDir ! OrderCanceled(ao, isSystemCancel, System.currentTimeMillis)
    }

    def executeOrder(s: AcceptedOrder, c: LimitOrder): OrderExecuted = {
      val oe = OrderExecuted(s, c, System.currentTimeMillis, s.matcherFee, c.matcherFee)
      addressDir ! oe
      oe
    }

    def updateBalances(changes: Map[Asset, Long]): Unit = {

      val updatedPortfolio = Portfolio(changes.getOrElse(Waves, 0), LeaseBalance.empty, changes.collect { case (a: IssuedAsset, b) => a -> b })
      val prevPortfolio    = Option(currentPortfolio getAndSet updatedPortfolio).getOrElse(Portfolio.empty)

      val spendableBalanceChanges: Map[Asset, Long] =
        prevPortfolio
          .changedAssetIds(updatedPortfolio)
          .map(asset => asset -> updatedPortfolio.spendableBalanceOf(asset))
          .toMap
          .withDefaultValue(0)

      spendableBalancesActor ! SpendableBalancesActor.Command.UpdateStates(Map(address.toAddress -> spendableBalanceChanges))
    }

    def expectWebSocketBalance(expected: Map[Asset, WsBalances]): Unit = {
      eventsProbe.expectMsgAnyClassOf(2.seconds, classOf[WsAddressState]).balances should matchTo(expected)
    }

    f(
      addressDir,
      eventsProbe,
      address,
      subscribe,
      addOrder,
      cancelOrder,
      executeOrder,
      updateBalances,
      expectWebSocketBalance
    )

    addressDir ! PoisonPill
  }

  "Actors web socket interaction" should {
    "correctly process web socket requests" when {

      "sender places order and then cancel it" in webSocketTest {
        (_, _, address, subscribeAddress, addOrder, cancel, _, updateBalances, expectWsBalance) =>
          updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))

          subscribeAddress
          expectWsBalance(Map(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0)))

          val order = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = address))

          addOrder(order)
          expectWsBalance(Map(usd -> WsBalances(285.usd, 15.usd)))

          cancel(order, false)
          expectWsBalance(Map(usd -> WsBalances(300.usd, 0)))
      }

      "sender places order, receives updates by third asset, partly fills order and then cancels remaining" in webSocketTest {
        (_, _, address, subscribeAddress, addOrder, cancel, executeOrder, updateBalances, expectWsBalance) =>
          withClue("Sender has 100 Waves and 300 USD, requests snapshot\n") {
            updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))
            subscribeAddress
            expectWsBalance(
              Map(
                Waves -> WsBalances(100.waves, 0),
                usd   -> WsBalances(300.usd, 0)
              )
            )
          }

          val buyOrder  = LimitOrder(createOrder(wavesUsdPair, BUY, 10.waves, 3.0, sender = address))
          val sellOrder = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0))

          withClue("Sender places order BUY 10 Waves\n") {
            addOrder(buyOrder)
            expectWsBalance(Map(usd -> WsBalances(270.usd, 30.usd)))
          }

          withClue("Sender received some ETH and this transfer transaction was forged\n") {
            updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 4.eth))
            expectWsBalance(Map(eth  -> WsBalances(4.eth, 0)))
          }

          val oe = executeOrder(sellOrder, buyOrder)

          withClue("Sender's order was partly filled by SELL 5 Waves. Balance changes are not atomic\n") {
            expectWsBalance(Map(usd   -> WsBalances(270.usd, 15.usd))) // first we send decreased balances
            updateBalances(Map(Waves  -> 105.waves, usd -> 285.usd, eth -> 4.eth)) // then we receive balance changes from blockchain
            expectWsBalance(Map(Waves -> WsBalances(105.waves, 0)))
          }

          withClue("Cancelling remaining of the counter order\n") {
            cancel(oe.counterRemaining, false)
            expectWsBalance(Map(usd -> WsBalances(285.usd, 0)))
          }
      }

      "sender places market order in nonempty order book, fee in ETH" in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalance) =>
          def matchOrders(submittedMarket: MarketOrder, counterAmount: Long): MarketOrder = {
            executeOrder(submittedMarket, LimitOrder(createOrder(wavesUsdPair, SELL, counterAmount, 3.00))).submittedMarketRemaining(submittedMarket)
          }

          val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)
          updateBalances(tradableBalance)

          subscribeAddress
          expectWsBalance { Map(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0), eth -> WsBalances(2.eth, 0)) }

          var mo = MarketOrder(createOrder(wavesUsdPair, BUY, 50.waves, 3.0, 0.00001703.eth, feeAsset = eth, sender = address), tradableBalance)

          withClue("Placing market order, reserves: 150 USD and 0.00001703 ETH\n") {
            placeOrder(mo)
            expectWsBalance { Map(usd -> WsBalances(150.usd, 150.usd), eth -> WsBalances(1.99998297.eth, 0.00001703.eth)) }
          }

          withClue("1 counter (10 Waves), reserves: 150-30 USD, 0.00001703-(0.00001703/5 = 0.00000340) ETH, transaction is immediately forged\n") {
            mo = matchOrders(mo, 10.waves)
            expectWsBalance { Map(usd -> WsBalances(150.usd, 120.usd), eth -> WsBalances(1.99998297.eth, 0.00001363.eth)) }

            updateBalances(Map(Waves -> 110.waves, usd -> 270.usd, eth -> 1.99999660.eth)) // transaction forged
            expectWsBalance { Map(Waves -> WsBalances(110.waves, 0)) }
          }

          withClue("2 counter (15 Waves), reserves: 120-45 USD, 0.00001363-(0.00001703/(50/15)) = 0.00000510) ETH\n") {
            mo = matchOrders(mo, 15.waves)
            expectWsBalance { Map(usd -> WsBalances(150.usd, 75.usd), eth -> WsBalances(1.99998297.eth, 0.00000853.eth)) }

            updateBalances(Map(Waves -> 125.waves, usd -> 225.usd, eth -> 1.99999150.eth)) // transaction forged
            expectWsBalance { Map(Waves -> WsBalances(125.waves, 0)) }
          }

          withClue("3 counter (5 Waves), reserves: 75-15 USD, 0.00000853-(0.00001703/10) = 0.00000170) ETH\n") {
            mo = matchOrders(mo, 5.waves)
            expectWsBalance { Map(usd -> WsBalances(150.usd, 60.usd), eth -> WsBalances(1.99998297.eth, 0.00000683.eth)) }

            updateBalances(Map(Waves -> 130.waves, usd -> 210.usd, eth -> 1.99998980.eth)) // transaction forged
            expectWsBalance { Map(Waves -> WsBalances(130.waves, 0)) }
          }

          withClue("System cancel of the market order remaining\n") {
            cancel(mo, true)
            expectWsBalance { Map(usd -> WsBalances(210.usd, 0), eth -> WsBalances(1.99998980.eth, 0)) }
          }

        // TODO Use blockchain updates stream to solve tradable balances toggling! (Node v.1.1.8)

//          withClue(s"1 transaction forged\n") {
//            updateBalances(Map(Waves -> 110.waves, usd -> 270.usd, eth -> 1.99999660.eth))
//            expectWsBalance { Map(Waves -> WsBalances(110.waves, 0), usd -> WsBalances(270.usd, 0), eth -> WsBalances(1.99999660.eth, 0)) }
//          }
//
//          withClue(s"2 transaction forged\n") {
//            updateBalances(Map(Waves -> 125.waves, usd -> 225.usd, eth -> 1.99999150.eth))
//            expectWsBalance { Map(Waves -> WsBalances(125.waves, 0), usd -> WsBalances(225.usd, 0), eth -> WsBalances(1.99999150.eth, 0)) }
//          }
//
//          withClue(s"3 transaction forged\n") {
//            updateBalances(Map(Waves -> 130.waves, usd -> 210.usd, eth -> 1.99998980.eth))
//            expectWsBalance { Map(Waves -> WsBalances(130.waves, 0), usd -> WsBalances(210.usd, 0), eth -> WsBalances(1.99998980.eth, 0)) }
//          }
      }

      "there are few subscriptions from single address" in webSocketTest { (ad, _, address, _, addOrder, cancel, _, updateBalances, _) =>
        val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)
        updateBalances(tradableBalance)

        def subscribe(tp: TestProbe): Unit = ad.tell(AddressDirectory.Envelope(address, AddressActor.AddWsSubscription), tp.ref)

        def expectWsBalance(tp: TestProbe, expected: Map[Asset, WsBalances]): Unit =
          tp.expectMsgAnyClassOf(10.second, classOf[WsAddressState]).balances should matchTo(expected)

        val webSubscription     = TestProbe()
        val mobileSubscription  = TestProbe()
        val desktopSubscription = TestProbe()

        subscribe(webSubscription)
        expectWsBalance(webSubscription, Map(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0), eth -> WsBalances(2.eth, 0)))

        updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth) }
        expectWsBalance(webSubscription, Map(eth -> WsBalances(5.eth, 0)))

        subscribe(mobileSubscription)
        expectWsBalance(mobileSubscription, Map(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(300.usd, 0), eth -> WsBalances(5.eth, 0)))

        val order = LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = address))

        addOrder(order)
        Seq(webSubscription, mobileSubscription).foreach { expectWsBalance(_, Map(usd -> WsBalances(297.usd, 3.usd))) }

        subscribe(desktopSubscription)
        expectWsBalance(desktopSubscription, Map(Waves -> WsBalances(100.waves, 0), usd -> WsBalances(297.usd, 3.usd), eth -> WsBalances(5.eth, 0)))

        cancel(order, true)
        Seq(webSubscription, mobileSubscription, desktopSubscription).foreach { expectWsBalance(_, Map(usd -> WsBalances(300.usd, 0))) }
      }

      "so far unsubscribed address made some actions and then subscribes" in webSocketTest {
        (_, _, address, subscribeAddress, addOrder, _, _, updateBalances, expectWsBalance) =>
          updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth) }
          addOrder { LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = address)) }

          updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth) }
          updateBalances { Map(Waves -> 115.waves, usd -> 300.usd, eth -> 5.eth) }

          subscribeAddress
          expectWsBalance { Map(Waves -> WsBalances(115.waves, 0), usd -> WsBalances(297.usd, 3.usd), eth -> WsBalances(5.eth, 0)) }
      }

      "spendable balance is equal to reserved " in webSocketTest {
        (_, _, address, subscribeAddress, addOrder, _, _, updateBalances, expectWsBalance) =>
          updateBalances { Map(Waves -> 100.waves, btc -> 1.btc) }
          addOrder { LimitOrder(createOrder(btcUsdPair, SELL, 1.btc, 8776.0, sender = address)) }

          subscribeAddress
          expectWsBalance { Map(Waves -> WsBalances(99.997.waves, 0.003.waves), btc -> WsBalances(0, 1.btc)) }
      }
    }
  }
}
