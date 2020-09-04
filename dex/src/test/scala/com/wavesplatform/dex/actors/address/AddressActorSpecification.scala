package com.wavesplatform.dex.actors.address

import java.util.concurrent.atomic.AtomicReference

import akka.actor.testkit.typed.{scaladsl => typed}
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.kernel.Monoid
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.SpendableBalancesActor
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.actors.address.AddressActor.Query.GetTradableBalance
import com.wavesplatform.dex.actors.address.AddressActor.Reply.Balance
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderAddedReason}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Future

class AddressActorSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with DiffMatcherWithImplicits
    with MatcherSpecBase {

  private implicit val typedSystem                = system.toTyped
  private implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

  private val assetId     = ByteStr("asset".getBytes("utf-8"))
  override val matcherFee = 30000L

  private val sellTokenOrder1 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken1Portfolio = requiredPortfolio(sellTokenOrder1)

  private val sellTokenOrder2 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 2L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken2Portfolio = requiredPortfolio(sellTokenOrder2)

  private val sellWavesOrder = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(assetId)),
    orderType = OrderType.SELL,
    price = 100000000L,
    amount = 100L,
    timestamp = 3L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellWavesPortfolio = requiredPortfolio(sellWavesOrder)

  "AddressActorSpecification" should {
    "cancel orders" when {
      "asset balance changed" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
        val initPortfolio = sellToken1Portfolio

        updatePortfolio(initPortfolio)
        addOrder(LimitOrder(sellTokenOrder1))

        updatePortfolio(initPortfolio.copy(assets = Map.empty))
        eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      }

      "waves balance changed" when {
        "there are waves for fee" in wavesBalanceTest(restWaves = matcherFee)
        "there are no waves at all" in wavesBalanceTest(restWaves = 0L)

        def wavesBalanceTest(restWaves: Long): Unit = test { (_, eventsProbe, addOrder, updatePortfolio) =>
          val initPortfolio = sellWavesPortfolio
          updatePortfolio(initPortfolio)

          addOrder(LimitOrder(sellWavesOrder))

          updatePortfolio(initPortfolio.copy(balance = restWaves))
          eventsProbe.expectMsg(QueueEvent.Canceled(sellWavesOrder.assetPair, sellWavesOrder.id(), Source.BalanceTracking))
        }
      }

      "waves were leased" when {
        "there are waves for fee" in leaseTest(_ => matcherFee)
        "there are no waves at all" in leaseTest(_.spendableBalance)

        def leaseTest(leasedWaves: Portfolio => Long): Unit = test { (_, eventsProbe, addOrder, updatePortfolio) =>
          val initPortfolio = sellWavesPortfolio
          updatePortfolio(initPortfolio)

          addOrder(LimitOrder(sellWavesOrder))

          updatePortfolio(initPortfolio.copy(lease = LeaseBalance(0, leasedWaves(initPortfolio))))
          eventsProbe.expectMsg(QueueEvent.Canceled(sellWavesOrder.assetPair, sellWavesOrder.id(), Source.BalanceTracking))
        }
      }
    }

    "return reservable balance without excess assets" in test { (ref, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = sellToken1Portfolio
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))

      ref ! GetTradableBalance(Set(Waves))
      expectMsgPF(hint = "Balance") {
        case r: Balance => r should matchTo(Balance(Map(Waves -> 0L)))
        case _          =>
      }
    }

    "track canceled orders and don't cancel more on same BalanceUpdated message" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combine(sellToken1Portfolio, sellToken2Portfolio)
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellToken1Portfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))

      updatePortfolio(sellToken1Portfolio) // same event
      eventsProbe.expectNoMessage()
    }

    "cancel multiple orders" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellWavesPortfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))
    }

    "cancel only orders, those aren't fit" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      Seq(sellTokenOrder1, sellWavesOrder, sellTokenOrder2).foreach(o => addOrder(LimitOrder(o)))

      updatePortfolio(sellWavesPortfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))
    }

    "cancel expired orders" in test { (ref, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = sellToken1Portfolio
      updatePortfolio(initPortfolio)
      ref ! AddressDirectoryActor.StartSchedules

      val lo = LimitOrder(
        OrderV1(
          sender = privateKey("test"),
          matcher = PublicKey("matcher".getBytes("utf-8")),
          pair = AssetPair(Waves, IssuedAsset(assetId)),
          orderType = OrderType.BUY,
          price = 100000000L,
          amount = 100L,
          timestamp = System.currentTimeMillis(),
          expiration = System.currentTimeMillis() + 100,
          matcherFee = matcherFee
        ))
      addOrder(lo)

      eventsProbe.expectMsg(QueueEvent.Canceled(lo.order.assetPair, lo.id, Source.Expiration))
    }

    "should not send a message multiple times" in test { (ref, _, addOrder, updatePortfolio) =>
      val initPortfolio = sellToken1Portfolio
      updatePortfolio(initPortfolio)
      addOrder(LimitOrder(sellTokenOrder1))

      val subscription1 = typed.TestProbe[WsAddressChanges]("probe-1")
      ref ! AddressDirectoryActor.Envelope(sellTokenOrder1.sender.toAddress, AddressActor.WsCommand.AddWsSubscription(subscription1.ref))
      subscription1.receiveMessage()

      ref ! AddressDirectoryActor.Envelope(sellTokenOrder1.sender.toAddress, AddressActor.WsCommand.RemoveWsSubscription(subscription1.ref))

      val subscription2 = typed.TestProbe[WsAddressChanges]("probe-2")
      ref ! AddressDirectoryActor.Envelope(sellTokenOrder1.sender.toAddress, AddressActor.WsCommand.AddWsSubscription(subscription2.ref))
      subscription2.receiveMessage()

      updatePortfolio(initPortfolio.copy(balance = initPortfolio.balance + 1))
      subscription2.receiveMessage()
    }
  }

  /**
    * (updatedPortfolio: Portfolio, sendBalanceChanged: Boolean) => Unit
    */
  private def test(f: (ActorRef, TestProbe, AcceptedOrder => Unit, Portfolio => Unit) => Unit): Unit = {

    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = addr("test")

    def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = Future.successful {
      (currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance).view.filterKeys(assets.contains)).toMap
    }

    def allAssetsSpendableBalance: Address => Future[Map[Asset, Long]] = { _ =>
      Future.successful { (currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance)).toMap }
    }

    lazy val spendableBalancesActor =
      system.actorOf(
        Props(
          new SpendableBalancesActor(spendableBalances, allAssetsSpendableBalance, addressDir)
        )
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
            Future.successful { Some(QueueEventWithMeta(0L, 0L, event)) }
          },
          enableSchedules,
          spendableBalancesActor
        )
      )
    }

    lazy val addressDir = system.actorOf(Props(new AddressDirectoryActor(EmptyOrderDB, createAddressActor, None)))

    def addOrder(ao: AcceptedOrder): Unit = {
      addressDir ! AddressDirectoryActor.Envelope(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      ao match {
        case lo: LimitOrder =>
          eventsProbe.expectMsg(QueueEvent.Placed(lo))
          addressDir ! OrderAdded(lo, OrderAddedReason.RequestExecuted, System.currentTimeMillis)
        case mo: MarketOrder => eventsProbe.expectMsg(QueueEvent.PlacedMarket(mo))
      }
    }

    f(
      addressDir,
      eventsProbe,
      addOrder,
      updatedPortfolio => {
        val prevPortfolio = Option(currentPortfolio.getAndSet(updatedPortfolio)).getOrElse(Portfolio.empty)

        val spendableBalanceChanges: Map[Asset, Long] =
          prevPortfolio
            .changedAssetIds(updatedPortfolio)
            .map(asset => asset -> updatedPortfolio.spendableBalanceOf(asset))
            .toMap
            .withDefaultValue(0)

        spendableBalancesActor ! SpendableBalancesActor.Command.UpdateStates(Map(address -> spendableBalanceChanges))
      }
    )

    addressDir ! PoisonPill
  }

  private def requiredPortfolio(order: Order): Portfolio = {
    val b = LimitOrder(order).requiredBalance
    Portfolio(b.getOrElse(Waves, 0L), LeaseBalance.empty, b.collect { case (id @ IssuedAsset(_), v) => id -> v })
  }

  private def addr(seed: String): Address       = privateKey(seed).toAddress
  private def privateKey(seed: String): KeyPair = KeyPair(seed.getBytes("utf-8"))

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }
}
