package com.wavesplatform.dex

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.kernel.Monoid
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.OrderAdded
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, OrderBookAggregatedSnapshot}
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
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
    with MatcherSpecBase {

  private implicit val efc: ErrorFormatterContext = (_: Asset) => 8

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
        eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      }

      "waves balance changed" when {
        "there are waves for fee" in wavesBalanceTest(restWaves = matcherFee)
        "there are no waves at all" in wavesBalanceTest(restWaves = 0L)

        def wavesBalanceTest(restWaves: Long): Unit = test { (_, eventsProbe, addOrder, updatePortfolio) =>
          val initPortfolio = sellWavesPortfolio
          updatePortfolio(initPortfolio)

          addOrder(LimitOrder(sellWavesOrder))

          updatePortfolio(initPortfolio.copy(balance = restWaves))
          eventsProbe.expectMsg(QueueEvent.Canceled(sellWavesOrder.assetPair, sellWavesOrder.id()))
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
          eventsProbe.expectMsg(QueueEvent.Canceled(sellWavesOrder.assetPair, sellWavesOrder.id()))
        }
      }
    }

    "track canceled orders and don't cancel more on same BalanceUpdated message" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combine(sellToken1Portfolio, sellToken2Portfolio)
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellToken1Portfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))

      updatePortfolio(sellToken1Portfolio) // same event
      eventsProbe.expectNoMessage()
    }

    "cancel multiple orders" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellWavesPortfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "cancel only orders, those aren't fit" in test { (_, eventsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      Seq(sellTokenOrder1, sellWavesOrder, sellTokenOrder2).foreach(o => addOrder(LimitOrder(o)))

      updatePortfolio(sellWavesPortfolio)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "schedule expired order cancellation" in {
      pending
    }
  }

  /**
    * (updatedPortfolio: Portfolio, sendBalanceChanged: Boolean) => Unit
    */
  private def test(f: (ActorRef, TestProbe, AcceptedOrder => Unit, Portfolio => Unit) => Unit): Unit = {

    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = addr("test")

    def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance).filterKeys(assets.contains) }
    }

    def allAssetsSpendableBalance: Address => Future[Map[Asset, Long]] = { _ =>
      Future.successful { currentPortfolio.get().assets ++ Map(Waves -> currentPortfolio.get().balance) }
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
          _ => Future.successful(false),
          event => {
            eventsProbe.ref ! event
            Future.successful { Some(QueueEventWithMeta(0, 0, event)) }
          },
          _ => OrderBookAggregatedSnapshot.empty,
          false,
          spendableBalancesActor
        )
      )
    }

    lazy val addressDir = system.actorOf(Props(new AddressDirectory(EmptyOrderDB, createAddressActor, None)))

    def addOrder(ao: AcceptedOrder): Unit = {
      addressDir ! AddressDirectory.Envelope(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      ao.fold { lo =>
        eventsProbe.expectMsg(QueueEvent.Placed(lo))
        addressDir ! OrderAdded(lo, System.currentTimeMillis)
      } { mo =>
        eventsProbe.expectMsg(QueueEvent.PlacedMarket(mo))
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
