package com.wavesplatform.dex.api.ws

import akka.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe => TypedTestProbe}
import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.syntax.option._
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.address.AddressActor.BlockchainInteraction
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsMessage}
import com.wavesplatform.dex.db.EmptyOrderDb
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderAddedReason, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder, _}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.time.Time
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.util.Success

class ActorsWebSocketInteractionsSpecification
    extends TestKit(ActorSystem("ActorsWebSocketInteractionsSpecification"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with MatcherSpecBase {

  private val testKit = ActorTestKit()

  implicit private val efc: ErrorFormatterContext = ErrorFormatterContext.from(asset => getDefaultAssetDescriptions(asset).decimals)

  class WebSocketTestEnvironment {
    val commandsProbe: TestProbe = TestProbe()
    val wsEventsProbe: TypedTestProbe[WsMessage] = testKit.createTestProbe[WsMessage]()

    val currentPortfolio = new AtomicReference[Portfolio](Portfolio.empty)
    val address = KeyPair("test".getBytes)

    val blockchainInteraction = new BlockchainInteraction {

      override def getFullBalances(a: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates] =
        if (a == address.toAddress) Future.successful(
          AddressBalanceUpdates(
            regular = (currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance)).filter(_._2 > 0).toMap,
            outgoingLeasing = None,
            pessimisticCorrection = Map.empty
          )
        )
        else Future.failed(WavesNodeConnectionLostException("Node unavailable", new IllegalStateException))

    }

    def createAddressActor(address: Address, recovered: Boolean): Props =
      Props(
        new AddressActor(
          address,
          Time.system,
          EmptyOrderDb(),
          (_, _) => Future.successful(Right(())),
          command => {
            commandsProbe.ref ! command
            Future.successful(Some(ValidatedCommandWithMeta(0L, 0L, command)))
          },
          recovered,
          blockchainInteraction
        )(efc)
      )

    val addressDir = system.actorOf(Props(new AddressDirectoryActor(EmptyOrderDb(), createAddressActor, None, recovered = true)))

    def subscribeAddress(): WebSocketTestEnvironment = {
      addressDir ! AddressDirectoryActor.Command.ForwardMessage(address, AddressActor.WsCommand.AddWsSubscription(wsEventsProbe.ref))
      this
    }

    def expectWsBalancesAndOrders(
      expectedBalances: Map[Asset, WsBalances],
      expectedOrders: Seq[WsOrder],
      expectedUpdateId: Long
    ): WebSocketTestEnvironment = {
      val wsAddressChanges = wsEventsProbe.expectMessageType[WsAddressChanges]
      wsAddressChanges.balances should matchTo(expectedBalances)
      wsAddressChanges.orders should matchTo(expectedOrders)
      wsAddressChanges.updateId should matchTo(expectedUpdateId)
      this
    }

    def placeOrder(ao: AcceptedOrder): WebSocketTestEnvironment = {
      addressDir ! AddressDirectoryActor.Command.ForwardMessage(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      commandsProbe.expectMsg(ao match {
        case lo: LimitOrder => ValidatedCommand.PlaceOrder(lo); case mo: MarketOrder => ValidatedCommand.PlaceMarketOrder(mo)
      })
      addressDir ! AddressActor.Command.ApplyOrderBookAdded(OrderAdded(ao, OrderAddedReason.RequestExecuted, System.currentTimeMillis))

      this
    }

    def cancelOrder(ao: AcceptedOrder, unmatchable: Boolean): WebSocketTestEnvironment = {
      if (ao.isLimit) {
        val source =
          if (unmatchable) AddressActor.Command.Source.Request // Not important in this test suite
          else AddressActor.Command.Source.Request
        addressDir ! AddressDirectoryActor.Command.ForwardMessage(address, AddressActor.Command.CancelOrder(ao.id, source))
        commandsProbe.expectMsg(ValidatedCommand.CancelOrder(ao.order.assetPair, ao.id, source, Some(ao.order.sender.toAddress)))
      }

      val reason = if (unmatchable) Events.OrderCanceledReason.BecameUnmatchable else Events.OrderCanceledReason.RequestExecuted
      addressDir ! AddressActor.Command.ApplyOrderBookCanceled(OrderCanceled(ao, reason, System.currentTimeMillis))

      this
    }

    def executeOrder(s: AcceptedOrder, c: LimitOrder): OrderExecuted = {
      val (counterExecutedFee, submittedExecutedFee) = Fee.getMakerTakerFee(DynamicSettings.symmetric(0.003.waves))(s, c)
      val oe = OrderExecuted(s, c, System.currentTimeMillis, counterExecutedFee, submittedExecutedFee)
      addressDir ! AddressActor.Command.ApplyOrderBookExecuted(oe, none) // TODO
      oe
    }

    def updateBalances(changes: Map[Asset, Long]): WebSocketTestEnvironment = {

      val updatedPortfolio = Portfolio(changes.getOrElse(Waves, 0), LeaseBalance.empty, changes.collect { case (a: IssuedAsset, b) => a -> b })
      val prevPortfolioOpt = Option(currentPortfolio getAndSet updatedPortfolio)
      val prevPortfolio = prevPortfolioOpt.getOrElse(Portfolio.empty)

      val regularBalanceChanges: Map[Asset, Long] =
        prevPortfolio
          .changedAssetIds(updatedPortfolio)
          .map(asset => asset -> updatedPortfolio.spendableBalanceOf(asset))
          .toMap
          .withDefaultValue(0)

      val updates = AddressBalanceUpdates(
        regular = regularBalanceChanges,
        outgoingLeasing = None,
        pessimisticCorrection = Map.empty
      )

      addressDir ! AddressDirectoryActor.Command.ForwardMessage(
        address.toAddress,
        if (prevPortfolioOpt.isEmpty) AddressActor.Command.SetInitialBalances(Success(updates), 0)
        else AddressActor.Command.ChangeBalances(updates)
      )

      this
    }

    def kill(): Unit = addressDir ! PoisonPill
  }

  "Actors web socket interaction" should {
    "correctly process web socket requests" when {

      "brand new sender subscribes" in {
        val env = new WebSocketTestEnvironment()
        env
          .subscribeAddress()
          .expectWsBalancesAndOrders(Map.empty, Seq.empty, 0)
          .kill()
      }

      "sender places order and then cancel it" in {
        val env = new WebSocketTestEnvironment()
        val lo = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = env.address))

        env
          .updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0)),
            Seq.empty,
            0
          )
          .placeOrder(lo)
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(285, 15), Waves -> WsBalances(99.997, 0.003)),
            Seq(WsOrder.fromDomain(lo)),
            1
          )
          .cancelOrder(lo, false)
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(300, 0), Waves -> WsBalances(100, 0)),
            Seq(WsOrder(lo.id, status = OrderStatus.Cancelled.name.some)),
            2
          )
          .kill()
      }

      // DEX-989
      "sender places order, receives updates by third asset, partly fills order and then cancels remaining" in {
        val env = new WebSocketTestEnvironment()
        val buyOrder = LimitOrder(createOrder(wavesUsdPair, BUY, 10.waves, 3.0, sender = env.address))
        val sellOrder = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0))

        withClue("Sender has 100 Waves and 300 USD, requests snapshot\n") {
          env
            .updateBalances(Map(Waves -> 100.waves, usd -> 300.usd))
            .subscribeAddress()
            .expectWsBalancesAndOrders(
              Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0)),
              Seq.empty,
              0
            )
        }

        withClue("Sender places order BUY 10 Waves\n") {
          env
            .placeOrder(buyOrder)
            .expectWsBalancesAndOrders(
              Map(usd -> WsBalances(270, 30), Waves -> WsBalances(99.997, 0.003)),
              Seq(WsOrder.fromDomain(buyOrder)),
              1
            )
        }

        withClue("Sender received some ETH and this transfer transaction was confirmed\n") {
          env
            .updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 4.eth))
            .expectWsBalancesAndOrders(
              Map(eth -> WsBalances(4, 0)),
              Seq.empty,
              2
            )
        }

        val oe = env.executeOrder(sellOrder, buyOrder)

        withClue("Sender's order was partly filled by SELL 5 Waves. Balance changes are not atomic\n") {
          // first we send decreased balances
          env
            .expectWsBalancesAndOrders(
              // The tradable balance will be changed to 270 USD and 99.997 WAVES when the exchange transaction comes to UTX
              // The half of order is still available
              Map(usd -> WsBalances(285, 15), Waves -> WsBalances(99.9985, 0.0015)),
              Seq(
                WsOrder(
                  id = buyOrder.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 5.0.some,
                  filledFee = 0.0015.some,
                  avgWeighedPrice = 3.0.some,
                  totalExecutedPriceAssets = 15.0.some
                )
              ),
              3
            )

            // 104.9985 = 100 + (10 - 0.003)/2
            .updateBalances(Map(Waves -> 104.9985.waves, usd -> 285.usd, eth -> 4.eth)) // The exchange transaction in the blockchain
            .expectWsBalancesAndOrders(
              Map(
                usd -> WsBalances(270, 15),
                Waves -> WsBalances(104.997, 0.0015)
              ),
              Seq.empty,
              4
            )
        }

        withClue("Cancelling remaining of the counter order\n") {
          env
            .cancelOrder(oe.counterRemaining, false)
            .expectWsBalancesAndOrders(
              Map(
                usd -> WsBalances(285, 0),
                Waves -> WsBalances(104.9985, 0)
              ),
              Seq(
                WsOrder(
                  id = buyOrder.id,
                  status = OrderStatus.Cancelled.name.some
                )
              ),
              5
            )
        }

        env.kill()
      }

      "sender places market order in nonempty order book, fee in ETH" in {
        val env = new WebSocketTestEnvironment()

        def matchOrders(submittedMarket: MarketOrder, counterAmount: Long): MarketOrder =
          env.executeOrder(submittedMarket, LimitOrder(createOrder(wavesUsdPair, SELL, counterAmount, 3.0))).submittedMarketRemaining(
            submittedMarket
          )

        val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 3.eth)

        env
          .updateBalances(tradableBalance)
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(3, 0)),
            Seq.empty,
            0
          )

        var mo = MarketOrder(createOrder(wavesUsdPair, BUY, 50.waves, 3.0, 1.eth, feeAsset = eth, sender = env.address), tradableBalance)

        withClue("Placing market order, reserves: 150 USD and 1 ETH\n") {
          env
            .placeOrder(mo)
            .expectWsBalancesAndOrders(
              Map(usd -> WsBalances(150, 150), eth -> WsBalances(2, 1)),
              Seq(WsOrder.fromDomain(mo)),
              1
            )
        }

        withClue("1 counter (+10 Waves, -30 USD, -0.2 ETH), reserves: 150 -> 120 USD, 1 -> 0.8 ETH, transaction is immediately confirmed\n") {
          mo = matchOrders(mo, 10.waves)
          env
            .expectWsBalancesAndOrders(
              // tradable = total - reserved, so 180 = 300 - 120 USD, 2.2 = 3 - 0.8 ETH
              Map(usd -> WsBalances(180, 120), eth -> WsBalances(2.2, 0.8)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 10.0.some,
                  filledFee = 0.2.some,
                  avgWeighedPrice = 3.0.some,
                  totalExecutedPriceAssets = 30.0.some
                )
              ),
              2
            )
        }

        withClue("2 counter (+15 Waves, -45 USD, -0.3 ETH), reserves: 120 -> 75 USD, 0.8 -> 0.5 ETH\n") {
          mo = matchOrders(mo, 15.waves)
          env
            .expectWsBalancesAndOrders(
              Map(usd -> WsBalances(225, 75), eth -> WsBalances(2.5, 0.5)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 25.0.some,
                  filledFee = 0.5.some,
                  avgWeighedPrice = 3.0.some,
                  totalExecutedPriceAssets = 75.0.some
                )
              ),
              3
            )
        }

        withClue("3 counter (+5 Waves, -15 USD, -0.1 ETH), reserves: 75 -> 60 USD, 0.5 -> 0.4 ETH\n") {
          mo = matchOrders(mo, 5.waves)
          env
            .expectWsBalancesAndOrders(
              Map(usd -> WsBalances(240, 60), eth -> WsBalances(2.6, 0.4)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 30.0.some,
                  filledFee = 0.6.some,
                  avgWeighedPrice = 3.0.some,
                  totalExecutedPriceAssets = 90.0.some
                )
              ),
              4
            )
        }

        withClue("System cancel of the market order remaining\n") {
          env
            .cancelOrder(mo, true)
            .expectWsBalancesAndOrders(
              Map(usd -> WsBalances(300, 0), eth -> WsBalances(3, 0)),
              Seq(
                WsOrder(
                  id = mo.id,
                  status = OrderStatus.Filled.name.some
                )
              ),
              5
            )
        }

        withClue(s"Exchange transactions are confirmed and balance changes are sent in one batch") {
          // TODO If balance changes aren't sent in one batch it could lead to tradable balance toggling! Use blockchain updates stream to solve this (Node v.1.2)
          env
            .updateBalances(Map(Waves -> 130.waves, usd -> 210.usd, eth -> 2.eth))
            .expectWsBalancesAndOrders(
              Map(Waves -> WsBalances(130, 0), usd -> WsBalances(210, 0), eth -> WsBalances(2, 0)),
              Seq.empty,
              6
            )
        }

        env.kill()
      }

      // DEX-989
      "there are few subscriptions from single address" in {
        val env = new WebSocketTestEnvironment()
        val tradableBalance = Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth)

        def subscribe(tp: TypedTestProbe[WsMessage]): Unit =
          env.addressDir ! AddressDirectoryActor.Command.ForwardMessage(env.address, AddressActor.WsCommand.AddWsSubscription(tp.ref))

        def expectWsBalance(tp: TypedTestProbe[WsMessage], expected: Map[Asset, WsBalances], expectedUpdateId: Long): Unit = {
          val wsAddressChanges = tp.expectMessageType[WsAddressChanges]
          wsAddressChanges.balances should matchTo(expected)
          wsAddressChanges.updateId should matchTo(expectedUpdateId)
        }

        env.updateBalances(tradableBalance)

        val webSubscription, mobileSubscription, desktopSubscription = testKit.createTestProbe[WsMessage]()

        subscribe(webSubscription)
        expectWsBalance(webSubscription, Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(2, 0)), 0)

        env.updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth))
        expectWsBalance(webSubscription, Map(eth -> WsBalances(5, 0)), 1)

        subscribe(mobileSubscription)
        expectWsBalance(mobileSubscription, Map(Waves -> WsBalances(100, 0), usd -> WsBalances(300, 0), eth -> WsBalances(5, 0)), 0)

        val order = LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = env.address))

        env.placeOrder(order)
        expectWsBalance(webSubscription, Map(usd -> WsBalances(297, 3), Waves -> WsBalances(99.997, 0.003)), 2)
        expectWsBalance(mobileSubscription, Map(usd -> WsBalances(297, 3), Waves -> WsBalances(99.997, 0.003)), 1)

        subscribe(desktopSubscription)
        expectWsBalance(desktopSubscription, Map(Waves -> WsBalances(99.997, 0.003), usd -> WsBalances(297, 3), eth -> WsBalances(5, 0)), 0)

        env.cancelOrder(order, true)

        expectWsBalance(webSubscription, Map(usd -> WsBalances(300, 0), Waves -> WsBalances(100, 0)), 3)
        expectWsBalance(mobileSubscription, Map(usd -> WsBalances(300, 0), Waves -> WsBalances(100, 0)), 2)
        expectWsBalance(desktopSubscription, Map(usd -> WsBalances(300, 0), Waves -> WsBalances(100, 0)), 1)

        env.kill()
      }

      "so far unsubscribed address made some actions and then subscribes" in {
        val env = new WebSocketTestEnvironment()
        val lo = LimitOrder(createOrder(wavesUsdPair, BUY, 1.waves, 3.0, sender = env.address))

        env
          .updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 2.eth))
          .placeOrder(lo)
          .updateBalances(Map(Waves -> 100.waves, usd -> 300.usd, eth -> 5.eth))
          .updateBalances(Map(Waves -> 115.waves, usd -> 300.usd, eth -> 5.eth))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(114.997, 0.003), usd -> WsBalances(297, 3), eth -> WsBalances(5, 0)),
            Seq(WsOrder.fromDomain(lo)),
            0
          )
          .kill()
      }

      "spendable balance is equal to reserved" in {
        val env = new WebSocketTestEnvironment()
        val lo = LimitOrder(createOrder(btcUsdPair, SELL, 1.btc, 8776.0, sender = env.address))

        env
          .updateBalances(Map(Waves -> 100.waves, btc -> 1.btc))
          .placeOrder(lo)
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(99.997, 0.003), btc -> WsBalances(0, 1)),
            Seq(WsOrder.fromDomain(lo)),
            0
          )
          .kill()
      }

      // DEX-989
      "order executes right after it was placed" ignore {
        val env = new WebSocketTestEnvironment()
        val counter = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0))
        val submitted = LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 3.0, sender = env.address))

        env
          .updateBalances(Map(Waves -> 100.waves, btc -> 1.btc))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0)),
            Seq.empty,
            0
          )

        val now = System.currentTimeMillis()

        env.addressDir ! OrderAdded(counter, OrderAddedReason.RequestExecuted, now)
        env.addressDir ! AddressDirectoryActor.Command.ForwardMessage(
          env.address,
          AddressActor.Command.PlaceOrder(submitted.order, submitted.isMarket)
        )
        env.commandsProbe.expectMsg(ValidatedCommand.PlaceOrder(submitted))
        env.addressDir ! OrderAdded(submitted, OrderAddedReason.RequestExecuted, now)

        val oe = OrderExecuted(submitted, counter, System.currentTimeMillis, submitted.matcherFee, counter.matcherFee)

        env.addressDir ! oe

        env
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0)),
            Seq(WsOrder.fromDomain(oe.submittedRemaining)),
            1
          )
          .kill()
      }

      "trade with itself" in {
        val env = new WebSocketTestEnvironment()
        val counter = LimitOrder(createOrder(wavesBtcPair, BUY, 5.waves, 3.0, sender = env.address))
        val submitted = LimitOrder(createOrder(wavesBtcPair, SELL, 5.waves, 3.0, sender = env.address))

        env
          .updateBalances(Map(Waves -> 100.waves, btc -> 1.btc))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0)),
            Seq.empty,
            0
          )

        val now = System.currentTimeMillis()
        env.addressDir ! AddressActor.Command.ApplyOrderBookAdded(OrderAdded(counter, OrderAddedReason.RequestExecuted, now))

        env.addressDir ! AddressDirectoryActor.Command.ForwardMessage(
          env.address,
          AddressActor.Command.PlaceOrder(submitted.order, submitted.isMarket)
        )
        env.commandsProbe.expectMsg(ValidatedCommand.PlaceOrder(submitted))
        env.addressDir ! AddressActor.Command.ApplyOrderBookAdded(OrderAdded(submitted, OrderAddedReason.RequestExecuted, now))

        val oe = OrderExecuted(submitted, counter, System.currentTimeMillis, counter.matcherFee, submitted.matcherFee)
        env.addressDir ! AddressActor.Command.ApplyOrderBookExecuted(oe, none)

        env
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0)),
            Seq(WsOrder.fromDomain(oe.counterRemaining), WsOrder.fromDomain(oe.submittedRemaining)),
            1
          )
          .kill()
      }

      "market order executes (address is sender of counters)" in {
        val env = new WebSocketTestEnvironment()
        val counter1 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0, sender = env.address))
        val counter2 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.1, sender = env.address))
        val counter3 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.2, sender = env.address))
        var mo = MarketOrder(createOrder(wavesUsdPair, SELL, 12.waves, 3.0), _ => Long.MaxValue)

        def matchOrders(submittedMarket: MarketOrder, counter: LimitOrder): (MarketOrder, LimitOrder) = {
          val oe = env.executeOrder(submittedMarket, counter)
          oe.submittedMarketRemaining(submittedMarket) -> oe.counterRemaining
        }

        env
          .updateBalances(Map(Waves -> 100.waves, usd -> 70.usd))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(70, 0), Waves -> WsBalances(100, 0)),
            Seq.empty,
            0
          )
          .placeOrder(counter1)
          .expectWsBalancesAndOrders(Map(usd -> WsBalances(55, 15), Waves -> WsBalances(99.997, 0.003)), Seq(WsOrder.fromDomain(counter1)), 1)
          .placeOrder(counter2)
          .expectWsBalancesAndOrders(Map(usd -> WsBalances(39.5, 30.5), Waves -> WsBalances(99.994, 0.006)), Seq(WsOrder.fromDomain(counter2)), 2)
          .placeOrder(counter3)
          .expectWsBalancesAndOrders(Map(usd -> WsBalances(23.5, 46.5), Waves -> WsBalances(99.991, 0.009)), Seq(WsOrder.fromDomain(counter3)), 3)

        mo = matchOrders(mo, counter1)._1

        env
          .expectWsBalancesAndOrders(
            Map(
              // executed = 5, reserved_diff = -15 = -5 * 3, reserved = 31.5 = 46.5 - 15, tradable = 38.5 = 70 - 31.5
              usd -> WsBalances(38.5, 31.5),
              Waves -> WsBalances(99.994, 0.006) // 0.006 is a commission for counter2 + counter3, 99.994 = 100 - 0.006
            ),
            Seq(
              WsOrder(
                id = counter1.id,
                status = OrderStatus.Filled.name.some,
                filledAmount = 5.0.some,
                filledFee = 0.003.some,
                avgWeighedPrice = 3.0.some,
                totalExecutedPriceAssets = 15.0.some
              )
            ),
            4
          )

        mo = matchOrders(mo, counter2)._1
        env
          .expectWsBalancesAndOrders(
            Map(
              // executed = 5, reserved_diff = -15.5 = -5 * 3.1, reserved = 16.5 = 31.5 - 15.5, tradable = 54 = 70 - 16
              usd -> WsBalances(54, 16),
              Waves -> WsBalances(99.997, 0.003)
            ),
            Seq(
              WsOrder(
                id = counter2.id,
                status = OrderStatus.Filled.name.some,
                filledAmount = 5.0.some,
                filledFee = 0.003.some,
                avgWeighedPrice = 3.1.some,
                totalExecutedPriceAssets = 15.5.some
              )
            ),
            5
          )

        val (_, counter3Remaining) = matchOrders(mo, counter3)

        env
          .expectWsBalancesAndOrders(
            Map(
              // executed = 2 = 12 - 5 - 5, reserved_diff = -6.4 = -2 * 3.2, reserved = 9.6 = 16 - 6.4, tradable = 60.4 = 70 - 9.6
              usd -> WsBalances(60.4, 9.6),
              Waves -> WsBalances(99.9982, 0.0018) // executed_fee = 0.0012 = 0.003 * 2 / 5
            ),
            Seq(
              WsOrder(
                id = counter3.id,
                status = OrderStatus.PartiallyFilled.name.some,
                filledAmount = 2.0.some,
                filledFee = 0.0012.some,
                avgWeighedPrice = 3.2.some,
                totalExecutedPriceAssets = 6.4.some
              )
            ),
            6
          )
          .cancelOrder(counter3Remaining, false)
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(70, 0), Waves -> WsBalances(100, 0)),
            Seq(WsOrder(id = counter3.id, status = OrderStatus.Cancelled.name.some)),
            7
          )
          .updateBalances(Map(usd -> 33.1.usd, Waves -> 111.9928.waves))
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(33.1, 0), Waves -> WsBalances(111.9928, 0)),
            Seq.empty,
            8
          )
          .kill()
      }

      // DEX-989
      "market order executes (address is sender of market)" ignore {
        val env = new WebSocketTestEnvironment()
        val counter1 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.0))
        val counter2 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.1))
        val counter3 = LimitOrder(createOrder(wavesUsdPair, BUY, 5.waves, 3.2))
        var mo = MarketOrder(createOrder(wavesUsdPair, SELL, 12.waves, 3.0, sender = env.address), _ => Long.MaxValue)

        def matchOrders(submittedMarket: MarketOrder, counter: LimitOrder): MarketOrder =
          env.executeOrder(submittedMarket, counter).submittedMarketRemaining(submittedMarket)

        env
          .updateBalances(Map(Waves -> 100.waves, usd -> 70.usd))
          .subscribeAddress()
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(100, 0), usd -> WsBalances(70, 0)),
            Seq.empty,
            0
          )
          .placeOrder(mo)
          .expectWsBalancesAndOrders(
            Map(Waves -> WsBalances(87.997, 12.003)),
            Seq(WsOrder.fromDomain(mo)),
            1
          )

        mo = matchOrders(mo, counter1)

        env
          .expectWsBalancesAndOrders(
            // executed = 5, executed_fee = 0.003 * 5 / 12 = 0.00125, reserved = 7.00175 = 12.003 - 5 - 0.00125, tradable = 92.99825 = 100 - 7.00175
            Map(Waves -> WsBalances(92.99825, 7.00175)),
            Seq(
              WsOrder(
                id = mo.id,
                status = OrderStatus.PartiallyFilled.name.some,
                filledAmount = 5.0.some,
                filledFee = 0.00125.some,
                avgWeighedPrice = 3.0.some,
                totalExecutedPriceAssets = 15.0.some
              )
            ),
            2
          )

        mo = matchOrders(mo, counter2)

        env
          .expectWsBalancesAndOrders(
            // executed = 5, executed_fee = 0.003 * 5 / 12 = 0.00125, reserved = 2.0005 = 7.00175 - 5 - 0.00125, tradable = 97.9995 = 100 - 2.0005
            Map(Waves -> WsBalances(97.9995, 2.0005)),
            Seq(
              WsOrder(
                id = mo.id,
                status = OrderStatus.PartiallyFilled.name.some,
                filledAmount = 10.0.some,
                filledFee = 0.0025.some,
                avgWeighedPrice = 3.05.some,
                totalExecutedPriceAssets = 30.5.some
              )
            ),
            3
          )

        matchOrders(mo, counter3)

        env
          .expectWsBalancesAndOrders(
            // executed = 2, executed_fee = 0.003 * 2 / 12 = 0.0005, reserved = 0 = 2.0005 - 2 - 0.0005, tradable = 100
            Map(Waves -> WsBalances(100, 0)),
            Seq(
              WsOrder(
                id = mo.id,
                status = OrderStatus.Filled.name.some,
                filledAmount = 12.0.some,
                filledFee = 0.003.some,
                avgWeighedPrice = 3.07.some,
                totalExecutedPriceAssets = 36.9.some
              )
            ),
            4
          )
          .updateBalances(Map(Waves -> 87.997.waves, usd -> 36.9.usd))
          .expectWsBalancesAndOrders(
            Map(usd -> WsBalances(36.9, 0), Waves -> WsBalances(87.997, 0)),
            Seq.empty,
            5
          )
          .kill()
      }
    }

    "correctly process order partially filling" in {
      val env = new WebSocketTestEnvironment()
      val bo = LimitOrder(createOrder(wavesUsdPair, BUY, 10.waves, 1.0, sender = env.address))

      env
        .updateBalances(Map(usd -> 10.usd, Waves -> 10.waves))
        .subscribeAddress()
        .expectWsBalancesAndOrders(
          Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)),
          Seq.empty,
          0
        )
        .placeOrder(bo)
        .expectWsBalancesAndOrders(
          Map(usd -> WsBalances(0, 10), Waves -> WsBalances(9.997, 0.003)),
          Seq(WsOrder.fromDomain(bo)),
          1
        )

      val oe = env.executeOrder(LimitOrder(createOrder(wavesUsdPair, SELL, 5.waves, 1.0)), bo)

      env
        .expectWsBalancesAndOrders(
          Map(usd -> WsBalances(5, 5), Waves -> WsBalances(9.9985, 0.0015)),
          Seq(
            WsOrder(
              id = bo.id,
              status = OrderStatus.PartiallyFilled.name,
              filledAmount = 5.0,
              filledFee = 0.0015,
              avgWeighedPrice = 1.0,
              totalExecutedPriceAssets = 5.0
            )
          ),
          2
        )
        .updateBalances(Map(Waves -> 14.9985.waves, usd -> 5.usd)) // +4.9985 Waves, since + 5 - 0.0015
        .expectWsBalancesAndOrders(
          Map(usd -> WsBalances(0, 5), Waves -> WsBalances(14.997, 0.0015)),
          Seq.empty,
          3
        )
        .cancelOrder(oe.counterRemaining, false)
        .expectWsBalancesAndOrders(
          Map(usd -> WsBalances(5, 0), Waves -> WsBalances(14.9985, 0)),
          Seq(WsOrder(id = bo.id, status = OrderStatus.Cancelled.name.some)),
          4
        )
        .kill()
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }

}
