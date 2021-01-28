package com.wavesplatform.dex.actors.address

import akka.actor.testkit.typed.{scaladsl => typed}
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.kernel.Monoid
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.address.AddressActor.BlockchainInteraction
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.actors.address.AddressActor.Query.{GetReservedBalance, GetTradableBalance}
import com.wavesplatform.dex.actors.address.AddressActor.Reply.GetBalance
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType, OrderV1}
import com.wavesplatform.dex.domain.state.{LeaseBalance, Portfolio}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderAddedReason}
import com.wavesplatform.dex.model.{AcceptedOrder, LimitOrder, MarketOrder}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

class AddressActorSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with DiffMatcherWithImplicits
    with MatcherSpecBase
    with Eventually {

  implicit private val typedSystem = system.toTyped
  implicit private val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

  private val assetId = ByteStr("asset".getBytes("utf-8"))
  override val matcherFee = 30000L

  private val sellTokenOrder1 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = System.currentTimeMillis(),
    expiration = System.currentTimeMillis() + 5.days.toMillis,
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
    timestamp = System.currentTimeMillis(),
    expiration = System.currentTimeMillis() + 5.days.toMillis,
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
    timestamp = System.currentTimeMillis(),
    expiration = System.currentTimeMillis() + 5.days.toMillis,
    matcherFee = matcherFee
  )

  private val sellWavesPortfolio = requiredPortfolio(sellWavesOrder)

  "AddressActorSpecification" should {
    val failed = Future.failed(new RuntimeException("test"))
    val kp = KeyPair(ByteStr("test".getBytes(StandardCharsets.UTF_8)))

    "request balances during a start" in {
      @volatile var requested = false

      def createAddressActor(address: Address, recovered: Boolean): Props =
        Props(
          new AddressActor(
            address,
            time,
            EmptyOrderDB,
            (_, _) => Future.successful(Right(())),
            _ => failed,
            recovered,
            (_: Address, _: Set[Asset]) => {
              requested = true
              failed
            }
          )
        )

      val addressDir = system.actorOf(Props(new AddressDirectoryActor(EmptyOrderDB, createAddressActor, None, recovered = false)))
      addressDir ! AddressDirectoryActor.Command.ForwardMessage(kp, AddressActor.Query.GetReservedBalance) // Creating an actor with kp's address
      eventually {
        requested shouldBe true
      }
    }

    "cancel orders" when {
      "asset balance changed" in test { (_, commandsProbe, addOrder, updatePortfolio) =>
        val initPortfolio = sellToken1Portfolio
        updatePortfolio(initPortfolio)

        addOrder(LimitOrder(sellTokenOrder1))

        updatePortfolio(initPortfolio.copy(assets = Map.empty))
        commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      }

      "waves balance changed" when {
        "there are waves for fee" in wavesBalanceTest(restWaves = matcherFee)
        "there are no waves at all" in wavesBalanceTest(restWaves = 0L)

        def wavesBalanceTest(restWaves: Long): Unit = test { (_, commandsProbe, addOrder, updatePortfolio) =>
          val initPortfolio = sellWavesPortfolio
          updatePortfolio(initPortfolio)

          addOrder(LimitOrder(sellWavesOrder))

          updatePortfolio(initPortfolio.copy(balance = restWaves))
          commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellWavesOrder.assetPair, sellWavesOrder.id(), Source.BalanceTracking))
        }
      }

      "waves were leased" when {
        "there are waves for fee" in leaseTest(_ => matcherFee)
        "there are no waves at all" in leaseTest(_.spendableBalance)

        def leaseTest(leasedWaves: Portfolio => Long): Unit = test { (_, commandsProbe, addOrder, updatePortfolio) =>
          val initPortfolio = sellWavesPortfolio
          updatePortfolio(initPortfolio)

          addOrder(LimitOrder(sellWavesOrder))

          updatePortfolio(initPortfolio.copy(lease = LeaseBalance(0, leasedWaves(initPortfolio))))
          commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellWavesOrder.assetPair, sellWavesOrder.id(), Source.BalanceTracking))
        }
      }
    }

    "return tradable balance" that {
      "without excess assets" in test { (ref, _, addOrder, updatePortfolio) =>
        updatePortfolio(sellToken1Portfolio.copy(balance = sellToken1Portfolio.balance + 1L))

        addOrder(LimitOrder(sellTokenOrder1))

        ref ! AddressDirectoryActor.Command.ForwardMessage(sellTokenOrder1.sender, GetTradableBalance(Set(Waves)))
        fishForSpecificMessage[GetBalance](hint = "Balance") {
          case x: GetBalance => x
        } should matchTo(GetBalance(Map[Asset, Long](Waves -> 1L)))
      }

      "without zero assets" in test { (ref, _, addOrder, updatePortfolio) =>
        updatePortfolio(sellToken1Portfolio)

        addOrder(LimitOrder(sellTokenOrder1))

        ref ! AddressDirectoryActor.Command.ForwardMessage(sellTokenOrder1.sender, GetTradableBalance(Set(Waves)))
        fishForSpecificMessage[GetBalance](hint = "Balance") {
          case x: GetBalance => x
        } should matchTo(GetBalance(Map.empty))
      }
    }

    "return reservable balance without excess assets" in test { (ref, _, addOrder, updatePortfolio) =>
      updatePortfolio(sellToken1Portfolio.copy(balance = sellToken1Portfolio.balance + 1L))

      addOrder(LimitOrder(sellTokenOrder1))

      ref ! AddressDirectoryActor.Command.ForwardMessage(sellTokenOrder1.sender, GetReservedBalance)
      fishForSpecificMessage[GetBalance](hint = "Balance") {
        case x: GetBalance => x
      } should matchTo(GetBalance(Map[Asset, Long](
        Waves -> 30000,
        sellTokenOrder1.assetPair.priceAsset -> 100
      )))
    }

    "track canceled orders and don't cancel more on same BalanceUpdated message" in test { (_, commandsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combine(sellToken1Portfolio, sellToken2Portfolio)
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellToken1Portfolio)
      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))

      updatePortfolio(sellToken1Portfolio) // same event
      commandsProbe.expectNoMessage()
    }

    "cancel multiple orders" in test { (_, commandsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      addOrder(LimitOrder(sellTokenOrder1))
      addOrder(LimitOrder(sellTokenOrder2))

      updatePortfolio(sellWavesPortfolio)
      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))
    }

    "cancel only orders, those aren't fit" in test { (_, commandsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellWavesPortfolio))
      updatePortfolio(initPortfolio)

      Seq(sellTokenOrder1, sellWavesOrder, sellTokenOrder2).foreach(o => addOrder(LimitOrder(o)))

      updatePortfolio(sellWavesPortfolio)
      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder1.assetPair, sellTokenOrder1.id(), Source.BalanceTracking))
      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(sellTokenOrder2.assetPair, sellTokenOrder2.id(), Source.BalanceTracking))
    }

    "cancel expired orders" in test { (_, commandsProbe, addOrder, updatePortfolio) =>
      val initPortfolio = sellToken1Portfolio
      updatePortfolio(initPortfolio)

      val lo = LimitOrder(
        OrderV1(
          sender = privateKey("test"),
          matcher = PublicKey("matcher".getBytes("utf-8")),
          pair = AssetPair(Waves, IssuedAsset(assetId)),
          orderType = OrderType.BUY,
          price = 100000000L,
          amount = 100L,
          timestamp = System.currentTimeMillis(),
          expiration = System.currentTimeMillis() + 100L,
          matcherFee = matcherFee
        )
      )
      addOrder(lo)

      commandsProbe.expectMsg(ValidatedCommand.CancelOrder(lo.order.assetPair, lo.id, Source.Expiration))
    }

    "should not send a message multiple times" in test { (ref, _, addOrder, updatePortfolio) =>
      val initPortfolio = sellToken1Portfolio
      updatePortfolio(initPortfolio)
      addOrder(LimitOrder(sellTokenOrder1))

      val subscription1 = typed.TestProbe[WsAddressChanges]("probe-1")
      ref ! AddressDirectoryActor.Command.ForwardMessage(
        sellTokenOrder1.sender.toAddress,
        AddressActor.WsCommand.AddWsSubscription(subscription1.ref)
      )
      subscription1.receiveMessage()

      ref ! AddressDirectoryActor.Command.ForwardMessage(
        sellTokenOrder1.sender.toAddress,
        AddressActor.WsCommand.RemoveWsSubscription(subscription1.ref)
      )

      val subscription2 = typed.TestProbe[WsAddressChanges]("probe-2")
      ref ! AddressDirectoryActor.Command.ForwardMessage(
        sellTokenOrder1.sender.toAddress,
        AddressActor.WsCommand.AddWsSubscription(subscription2.ref)
      )
      subscription2.receiveMessage()

      updatePortfolio(initPortfolio.copy(balance = initPortfolio.balance + 1))
      subscription2.receiveMessage()
    }
  }

  /**
   * (updatedPortfolio: Portfolio, sendBalanceChanged: Boolean) => Unit
   */
  private def test(f: (ActorRef, TestProbe, AcceptedOrder => Unit, Portfolio => Unit) => Unit): Unit = {

    val commandsProbe = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address = addr("test")

    val blockchainInteraction = new BlockchainInteraction {
      override def getFullBalances(address: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates] =
        Future.successful(
          AddressBalanceUpdates(
            regular = currentPortfolio.get().assets.toMap[Asset, Long].updated(Waves, currentPortfolio.get().balance),
            outgoingLeasing = None,
            pessimisticCorrection = Map.empty
          )
        )
    }

    def createAddressActor(address: Address, recovered: Boolean): Props =
      Props(
        new AddressActor(
          address,
          time,
          EmptyOrderDB,
          (_, _) => Future.successful(Right(())),
          command => {
            commandsProbe.ref ! command
            Future.successful(Some(ValidatedCommandWithMeta(0L, 0L, command)))
          },
          recovered,
          blockchainInteraction
        )
      )

    lazy val addressDir = system.actorOf(Props(new AddressDirectoryActor(EmptyOrderDB, createAddressActor, None, recovered = true)))

    def addOrder(ao: AcceptedOrder): Unit = {
      addressDir ! AddressDirectoryActor.Command.ForwardMessage(address, AddressActor.Command.PlaceOrder(ao.order, ao.isMarket))
      ao match {
        case lo: LimitOrder =>
          commandsProbe.expectMsg(ValidatedCommand.PlaceOrder(lo))
          addressDir ! AddressActor.Command.ApplyOrderBookAdded(OrderAdded(lo, OrderAddedReason.RequestExecuted, System.currentTimeMillis))
        case mo: MarketOrder => commandsProbe.expectMsg(ValidatedCommand.PlaceMarketOrder(mo))
      }
    }

    f(
      addressDir,
      commandsProbe,
      addOrder,
      updatedPortfolio => {
        val prevPortfolioOption = Option(currentPortfolio.getAndSet(updatedPortfolio))
        val prevPortfolio = prevPortfolioOption.getOrElse(Portfolio.empty)

        val regularBalanceChanges: Map[Asset, Long] =
          prevPortfolio
            .changedAssetIds(updatedPortfolio)
            .map(asset => asset -> updatedPortfolio.spendableBalanceOf(asset))
            .toMap
            .withDefaultValue(0)

        val changes = AddressBalanceUpdates(
          regular = regularBalanceChanges,
          outgoingLeasing = None,
          pessimisticCorrection = Map.empty
        )

        val message =
          if (prevPortfolioOption.isEmpty) AddressActor.Command.SetInitialBalances(Success(changes), 0)
          else AddressActor.Command.ChangeBalances(changes)

        addressDir ! AddressDirectoryActor.Command.ForwardMessage(address, message)
      }
    )

    addressDir ! PoisonPill
  }

  private def requiredPortfolio(order: Order): Portfolio = {
    val b = LimitOrder(order).requiredBalance
    Portfolio(b.getOrElse(Waves, 0L), LeaseBalance.empty, b.collect { case (id @ IssuedAsset(_), v) => id -> v })
  }

  private def addr(seed: String): Address = privateKey(seed).toAddress
  private def privateKey(seed: String): KeyPair = KeyPair(seed.getBytes("utf-8"))

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

}
