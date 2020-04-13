package com.wavesplatform.dex

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder, _}
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

  private implicit val efc: ErrorFormatterContext = (asset: Asset) => getDefaultAssetDescriptions(asset).decimals

  private def webSocketTest(
      f: (
          ActorRef, // address directory
          TestProbe, // test probe
          KeyPair, // owner's key pair
          () => Unit, // subscribe
          AcceptedOrder => Unit, // place order
          (AcceptedOrder, Boolean) => Unit, // cancel
          (AcceptedOrder, LimitOrder) => OrderExecuted, // execute
          Map[Asset, Long] => Unit, // update spendable balances
          (Map[Asset, WsBalances], Seq[WsOrder], Long) => Unit // expect balances diff, orders diff and update id by web socket
      ) => Unit
  ): Unit = {

    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = KeyPair("test".getBytes)

    def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance).filterKeys(assets) }
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
          (_, _) => Future.successful(Right(())),
          event => {
            eventsProbe.ref ! event
            Future.successful { Some(QueueEventWithMeta(0, 0, event)) }
          },
          enableSchedules,
          spendableBalancesActor
        )
      )
    }

    def subscribe(): Unit = addressDir.tell(AddressDirectory.Envelope(address, AddressActor.AddWsSubscription), eventsProbe.ref)

    def placeOrder(ao: AcceptedOrder): Unit = {
      addressDir ! AddressDirectory.Envelope(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      eventsProbe.expectMsg(ao match { case lo: LimitOrder => QueueEvent.Placed(lo); case mo: MarketOrder => QueueEvent.PlacedMarket(mo) })
      addressDir ! OrderAdded(ao, System.currentTimeMillis)
    }

    def cancelOrder(ao: AcceptedOrder, isSystemCancel: Boolean): Unit = {
      if (ao.isLimit) {
        addressDir ! AddressDirectory.Envelope(address, AddressActor.Command.CancelOrder(ao.id))
        eventsProbe.expectMsg(QueueEvent.Canceled(ao.order.assetPair, ao.id))
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

    def expectBalancesAndOrders(expectedBalances: Map[Asset, WsBalances], expectedOrders: Seq[WsOrder], expectedUpdateId: Long): Unit = {
      val wsAddressState = eventsProbe.expectMsgAnyClassOf(3.seconds, classOf[WsAddressState])
      wsAddressState.balances should matchTo(expectedBalances)
      wsAddressState.orders should matchTo(expectedOrders)
      wsAddressState.updateId should matchTo(expectedUpdateId)
    }

    f(
      addressDir,
      eventsProbe,
      address,
      subscribe,
      placeOrder,
      cancelOrder,
      executeOrder,
      updateBalances,
      expectBalancesAndOrders
    )

    addressDir ! PoisonPill
  }

  "Actors web socket interaction" should {
    "correctly process web socket requests" when {

      "sender places order and then cancel it" in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, cancel, _, updateBalances, expectWsBalancesAndOrders) =>
          updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0)),
            Seq.empty,
            0
          )

          val lo = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = address))

          placeOrder(lo)
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(285, 15)),
            Seq(WsOrder.fromDomain(lo, OrderStatus.Accepted)),
            1
          )

          cancel(lo, false)
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(300, 0)),
            Seq(WsOrder(lo.id, status = OrderStatus.Cancelled.name.some)),
            2
          )
      }

      "sender places order, receives updates by third asset, partly fills order and then cancels remaining" in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalancesAndOrders) =>
          withClue("Sender has 100 Waves and 300 USD, requests snapshot\n") {

            updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))
            subscribeAddress()

            expectWsBalancesAndOrders(
              Map(
                Waves -> WsBalances(100, 0),
                usd   -> WsBalances(300, 0)
              ),
              Seq.empty,
              0
            )
          }

          val buyOrder  = LimitOrder(createOrder(wavesUsdPair, BUY, 10.waves, 3.0, sender = address))
          val sellOrder = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0))

          withClue("Sender places order BUY 10 Waves\n") {
            placeOrder(buyOrder)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(270, 30)),
              Seq(WsOrder.fromDomain(buyOrder, OrderStatus.Accepted)),
              1
            )
          }

          withClue("Sender received some ETH and this transfer transaction was forged\n") {
            updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 4.eth))
            expectWsBalancesAndOrders(
              Map(eth -> WsBalances(4, 0)),
              Seq.empty,
              2
            )
          }

          val oe = executeOrder(sellOrder, buyOrder)

          withClue("Sender's order was partly filled by SELL 5 Waves. Balance changes are not atomic\n") {
            // first we send decreased balances
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(270, 15)),
              Seq(
                WsOrder(
                  id = buyOrder.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 5.0.some,
                  filledFee = 0.0015.some,
                  avgWeighedPrice = 3.0.some
                )
              ),
              3
            )

            updateBalances(Map(Waves -> 105.waves, usd -> 285.usd, eth -> 4.eth)) // then we receive balance changes from blockchain

            expectWsBalancesAndOrders(
              Map(Waves -> WsBalances(105, 0)),
              Seq.empty,
              4
            )
          }

          withClue("Cancelling remaining of the counter order\n") {
            cancel(oe.counterRemaining, false)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(285, 0)),
              Seq(
                WsOrder(
                  id = buyOrder.id,
                  status = OrderStatus.Cancelled.name.some
                )
              ),
              5
            )
          }
      }

      "sender places market order in nonempty order book, fee in ETH" in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalancesAndOrders) =>
          def matchOrders(submittedMarket: MarketOrder, counterAmount: Long): MarketOrder = {
            executeOrder(submittedMarket, LimitOrder(createOrder(wavesUsdPair, SELL, counterAmount, 3.0))).submittedMarketRemaining(submittedMarket)
          }

          val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)
          updateBalances(tradableBalance)

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(2, 0)),
            Seq.empty,
            0
          )

          var mo = MarketOrder(createOrder(wavesUsdPair, BUY, 50.waves, 3.0, 0.00001703.eth, feeAsset = eth, sender = address), tradableBalance)

          withClue("Placing market order, reserves: 150 USD and 0.00001703 ETH\n") {
            placeOrder(mo)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(150, 150), eth -> WsBalances(1.99998297, 0.00001703)),
              Seq(WsOrder.fromDomain(mo, OrderStatus.Accepted)),
              1
            )
          }

          withClue("1 counter (10 Waves), reserves: 150-30 USD, 0.00001703-(0.00001703/5 = 0.00000340) ETH, transaction is immediately forged\n") {
            mo = matchOrders(mo, 10.waves)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(150, 120), eth -> WsBalances(1.99998297, 0.00001363)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 10.0.some,
                  filledFee = 0.00000340.some,
                  avgWeighedPrice = 3.0.some
                )
              ),
              2
            )
          }

          withClue("2 counter (15 Waves), reserves: 120-45 USD, 0.00001363-(0.00001703/(50/15)) = 0.00000510) ETH\n") {
            mo = matchOrders(mo, 15.waves)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(150, 75), eth -> WsBalances(1.99998297, 0.00000853)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 25.0.some,
                  filledFee = 0.00000850.some,
                  avgWeighedPrice = 3.0.some
                )
              ),
              3
            )
          }

          withClue("3 counter (5 Waves), reserves: 75-15 USD, 0.00000853-(0.00001703/10) = 0.00000170) ETH\n") {
            mo = matchOrders(mo, 5.waves)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(150, 60), eth -> WsBalances(1.99998297, 0.00000683)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 30.0.some,
                  filledFee = 0.00001020.some,
                  avgWeighedPrice = 3.0.some
                )
              ),
              4
            )
          }

          withClue("System cancel of the market order remaining\n") {
            cancel(mo, true)
            expectWsBalancesAndOrders(
              Map(usd -> WsBalances(210, 0), eth -> WsBalances(1.99998980, 0)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.Filled.name.some
                )
              ),
              5
            )
          }

          withClue(s"Exchange transactions are forged and balance changes are sent in one batch") {
            // TODO If balance changes aren't sent in one batch it could lead to tradable balance toggling! Use blockchain updates stream to solve this (Node v.1.2)
            updateBalances(Map(Waves -> 130.waves, usd -> 210.usd, eth -> 1.99998980.eth))
            expectWsBalancesAndOrders(
              Map(Waves -> WsBalances(130, 0)),
              Seq.empty,
              6
            )
          }
      }

      "there are few subscriptions from single address" in webSocketTest { (ad, _, address, _, placeOrder, cancel, _, updateBalances, _) =>
        val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)
        updateBalances(tradableBalance)

        def subscribe(tp: TestProbe): Unit = ad.tell(AddressDirectory.Envelope(address, AddressActor.AddWsSubscription), tp.ref)

        def expectWsBalance(tp: TestProbe, expected: Map[Asset, WsBalances], expectedUpdateId: Long): Unit = {
          val wsAddressState = tp.expectMsgAnyClassOf(10.second, classOf[WsAddressState])
          wsAddressState.balances should matchTo(expected)
          wsAddressState.updateId should matchTo(expectedUpdateId)
        }

        val webSubscription     = TestProbe()
        val mobileSubscription  = TestProbe()
        val desktopSubscription = TestProbe()

        subscribe(webSubscription)
        expectWsBalance(webSubscription, Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(2, 0)), 0)

        updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth) }
        expectWsBalance(webSubscription, Map(eth -> WsBalances(5, 0)), 1)

        subscribe(mobileSubscription)
        expectWsBalance(mobileSubscription, Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(5, 0)), 0)

        val order = LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = address))

        placeOrder(order)
        expectWsBalance(webSubscription, Map(usd    -> WsBalances(297, 3)), 2)
        expectWsBalance(mobileSubscription, Map(usd -> WsBalances(297, 3)), 1)

        subscribe(desktopSubscription)
        expectWsBalance(desktopSubscription, Map(Waves -> WsBalances(100, 0), usd -> WsBalances(297, 3), eth -> WsBalances(5, 0)), 0)

        cancel(order, true)

        expectWsBalance(webSubscription, Map(usd     -> WsBalances(300, 0)), 3)
        expectWsBalance(mobileSubscription, Map(usd  -> WsBalances(300, 0)), 2)
        expectWsBalance(desktopSubscription, Map(usd -> WsBalances(300, 0)), 1)
      }

      "so far unsubscribed address made some actions and then subscribes" in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, _, _, updateBalances, expectWsBalancesAndOrders) =>
          updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth) }
          val lo = LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = address))

          placeOrder(lo)

          updateBalances { Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth) }
          updateBalances { Map(Waves -> 115.waves, usd -> 300.usd, eth -> 5.eth) }

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(115, 0), usd -> WsBalances(297, 3), eth -> WsBalances(5, 0)),
            Seq(WsOrder.fromDomain(lo, OrderStatus.Accepted)),
            0
          )
      }

      "spendable balance is equal to reserved " in webSocketTest {
        (_, _, address, subscribeAddress, placeOrder, _, _, updateBalances, expectWsBalancesAndOrders) =>
          updateBalances { Map(Waves -> 100.waves, btc -> 1.btc) }
          val lo = LimitOrder(createOrder(btcUsdPair, SELL, 1.btc, 8776.0, sender = address))
          placeOrder(lo)

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(99.997, 0.003), btc -> WsBalances(0, 1)),
            Seq(WsOrder.fromDomain(lo, OrderStatus.Accepted)),
            0
          )
      }

      "order executes right after it was placed" in webSocketTest {
        (ad, ep, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalancesAndOrders) =>
          updateBalances { Map(Waves -> 100.waves, btc -> 1.btc) }

          val counter   = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0))
          val submitted = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0, sender = address))

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0)),
            Seq.empty,
            0
          )

          val now = System.currentTimeMillis()
          ad ! OrderAdded(counter, now)

          ad ! AddressDirectory.Envelope(address, AddressActor.Command.PlaceOrder(submitted.order, submitted.isMarket))
          ep.expectMsg(QueueEvent.Placed(submitted))

          ad ! OrderAdded(submitted, now)
          val oe = OrderExecuted(submitted, counter, System.currentTimeMillis, submitted.matcherFee, counter.matcherFee)
          ad ! oe

          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(94.997, 0)),
            Seq(
              WsOrder.fromDomain(oe.submittedRemaining, OrderStatus.Filled(5.waves, 0.003.waves))
            ),
            1
          )
      }

      "market order executes (address is sender of counters)" in webSocketTest {
        (ad, ep, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalancesAndOrders) =>
          def matchOrders(submittedMarket: MarketOrder, counter: LimitOrder): (MarketOrder, LimitOrder) = {
            val oe = executeOrder(submittedMarket, counter)
            oe.submittedMarketRemaining(submittedMarket) -> oe.counterRemaining
          }

          updateBalances { Map(Waves -> 100.waves, usd -> 70.usd) }

          val counter1 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = address))
          val counter2 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.1, sender = address))
          val counter3 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.2, sender = address))

          var mo = MarketOrder(createOrder(wavesUsdPair, SELL, 12.waves, 3.0), _ => Long.MaxValue)

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(70, 0)),
            Seq.empty,
            0
          )

          placeOrder(counter1)
          expectWsBalancesAndOrders(Map(usd -> WsBalances(55, 15)), Seq(WsOrder.fromDomain(counter1, OrderStatus.Accepted)), 1)

          placeOrder(counter2)
          expectWsBalancesAndOrders(Map(usd -> WsBalances(39.5, 30.5)), Seq(WsOrder.fromDomain(counter2, OrderStatus.Accepted)), 2)

          placeOrder(counter3)
          expectWsBalancesAndOrders(Map(usd -> WsBalances(23.5, 46.5)), Seq(WsOrder.fromDomain(counter3, OrderStatus.Accepted)), 3)

          mo = matchOrders(mo, counter1)._1
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(23.5, 31.5)),
            Seq(
              WsOrder(id = counter1.id,
                      status = OrderStatus.Filled.name.some,
                      filledAmount = 5.0.some,
                      filledFee = 0.003.some,
                      avgWeighedPrice = 3.0.some)
            ),
            4
          )

          mo = matchOrders(mo, counter2)._1
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(23.5, 16)),
            Seq(
              WsOrder(id = counter2.id,
                      status = OrderStatus.Filled.name.some,
                      filledAmount = 5.0.some,
                      filledFee = 0.003.some,
                      avgWeighedPrice = 3.1.some)
            ),
            5
          )

          val (_, counter3Remaining) = matchOrders(mo, counter3)
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(23.5, 9.6)),
            Seq(
              WsOrder(id = counter3.id,
                      status = OrderStatus.PartiallyFilled.name.some,
                      filledAmount = 2.0.some,
                      filledFee = 0.0012.some,
                      avgWeighedPrice = 3.2.some)
            ),
            6
          )

          cancel(counter3Remaining, false)
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(33.1, 0)),
            Seq(
              WsOrder(id = counter3.id, status = OrderStatus.Cancelled.name.some)
            ),
            7
          )

          updateBalances(Map(Waves -> 111.9928.waves, usd -> 33.1.usd))
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(111.9928, 0)),
            Seq.empty,
            8
          )
      }

      "market order executes (address is sender of market)" in webSocketTest {
        (ad, ep, address, subscribeAddress, placeOrder, cancel, executeOrder, updateBalances, expectWsBalancesAndOrders) =>
          def matchOrders(submittedMarket: MarketOrder, counter: LimitOrder): MarketOrder = {
            executeOrder(submittedMarket, counter).submittedMarketRemaining(submittedMarket)
          }

          updateBalances { Map(Waves -> 100.waves, usd -> 70.usd) }

          val counter1 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0))
          val counter2 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.1))
          val counter3 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.2))

          var mo = MarketOrder(createOrder(wavesUsdPair, SELL, 12.waves, 3.0, sender = address), _ => Long.MaxValue)

          subscribeAddress()
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(70, 0)),
            Seq.empty,
            0
          )

          placeOrder(mo)
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(87.997, 12.003)),
            Seq(WsOrder.fromDomain(mo, OrderStatus.Accepted)),
            1
          )

          mo = matchOrders(mo, counter1)
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(87.997, 7.00175)),
            Seq(
              WsOrder(
                id = mo.id,
                status = OrderStatus.PartiallyFilled.name.some,
                filledAmount = 5.0.some,
                filledFee = 0.00125.some,
                avgWeighedPrice = 3.0.some
              )
            ),
            2
          )

          mo = matchOrders(mo, counter2)
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(87.997, 2.0005)),
            Seq(
              WsOrder(id = mo.id,
                      status = OrderStatus.PartiallyFilled.name.some,
                      filledAmount = 10.0.some,
                      filledFee = 0.0025.some,
                      avgWeighedPrice = 3.05.some)
            ),
            3
          )

          matchOrders(mo, counter3)
          expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(87.997, 0)),
            Seq(
              WsOrder(
                id = mo.id,
                status = OrderStatus.Filled.name.some,
                filledAmount = 12.0.some,
                filledFee = 0.003.some,
                avgWeighedPrice = 3.07.some
              )
            ),
            4
          )

          updateBalances(Map(Waves -> 87.997.waves, usd -> 36.9.usd))
          expectWsBalancesAndOrders(
            Map(usd -> WsBalances(36.9, 0)),
            Seq.empty,
            5
          )
      }
    }
  }
}
