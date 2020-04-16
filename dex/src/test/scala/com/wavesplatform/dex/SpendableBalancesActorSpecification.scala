package com.wavesplatform.dex

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import com.wavesplatform.dex.db.EmptyOrderDB
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.model.OrderBookAggregatedSnapshot
import com.wavesplatform.dex.queue.QueueEventWithMeta
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SpendableBalancesActorSpecification
    extends TestKit(ActorSystem("SpendableBalancesActorSpecification"))
    with AnyWordSpecLike
    with Matchers
    with MatcherSpecBase {

  implicit val efc: ErrorFormatterContext = (_: Asset) => 8

  val testProbe: TestProbe = TestProbe()

  val alice: Address = KeyPair("alice".getBytes).toAddress
  val bob: Address   = KeyPair("bob".getBytes).toAddress

  val balancesFromNode: Map[Address, Map[Asset, Long]] = Map(
    alice -> Map(Waves -> 100.waves, usd -> 50.usd, btc -> 2.btc),
    bob   -> Map(Waves -> 300.waves, eth -> 5.eth)
  )

  val spendableBalancesGrpcCalls = new ConcurrentHashMap[Address, Int](Map(alice -> 0, bob -> 0).asJava)

  def allAssetsSpendableBalances(address: Address): Future[Map[Asset, Long]] = Future.successful {
    balancesFromNode.getOrElse(address, Map.empty)
  }

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] = {
    spendableBalancesGrpcCalls.computeIfPresent(address, (_, pv) => pv + 1)
    allAssetsSpendableBalances(address).map { stateFromNode =>
      assets.map(a => a -> stateFromNode.getOrElse(a, 0L)).toMap
    }
  }

  lazy val ad: ActorRef  = system.actorOf(Props(new AddressDirectory(EmptyOrderDB, createAddressActor, None)))
  lazy val sba: ActorRef = system.actorOf(Props(new SpendableBalancesActor(spendableBalances, allAssetsSpendableBalances, ad)))

  def createAddressActor(address: Address, enableSchedules: Boolean): Props = {
    Props(
      new AddressActor(
        address,
        time,
        EmptyOrderDB,
        _ => Future.successful(false),
        event => { testProbe.ref ! event; Future.successful { Some(QueueEventWithMeta(0, 0, event)) } },
        _ => OrderBookAggregatedSnapshot.empty,
        enableSchedules,
        sba
      )
    )
  }

  "SpendableBalancesActor" should {

    "handle state queries without excess gRPC calls" in {

      def checkStateAndCalls(address: Address, assets: Set[Asset], callsCount: Int, expectedBalance: (Asset, Long)*): Unit = {
        sba.tell(SpendableBalancesActor.Query.GetState(address, assets), testProbe.ref)
        testProbe.expectMsgAnyClassOf(1.seconds, classOf[SpendableBalancesActor.Reply.GetState]).state should matchTo(expectedBalance.toMap)
        spendableBalancesGrpcCalls.get(address) shouldBe callsCount
      }

      sba ! SpendableBalancesActor.Command.UpdateStates(balancesFromNode.filterKeys(_ == bob))

      // format: off
      checkStateAndCalls(bob,   Set(Waves, eth), callsCount = 0, expectedBalance = Waves -> 300.waves, eth -> 5.eth)
      checkStateAndCalls(alice, Set(Waves),      callsCount = 1, expectedBalance = Waves -> 100.waves)
      checkStateAndCalls(alice, Set(Waves),      callsCount = 1, expectedBalance = Waves -> 100.waves)
      checkStateAndCalls(alice, Set(usd),        callsCount = 2, expectedBalance = usd -> 50.usd)
      checkStateAndCalls(alice, Set(usd),        callsCount = 2, expectedBalance = usd -> 50.usd)
      checkStateAndCalls(alice, Set(Waves, btc), callsCount = 3, expectedBalance = Waves -> 100.waves, btc -> 2.btc)
      checkStateAndCalls(alice, Set(Waves, btc), callsCount = 3, expectedBalance = Waves -> 100.waves, btc -> 2.btc)
      checkStateAndCalls(alice, Set(btc),        callsCount = 3, expectedBalance = btc -> 2.btc)
      checkStateAndCalls(alice, Set(eth),        callsCount = 4, expectedBalance = eth -> 0)
      checkStateAndCalls(alice, Set(eth),        callsCount = 4, expectedBalance = eth -> 0)
      checkStateAndCalls(bob,   Set(Waves, usd), callsCount = 1, expectedBalance = Waves -> 300.waves, usd -> 0)
      checkStateAndCalls(bob,   Set(Waves, usd), callsCount = 1, expectedBalance = Waves -> 300.waves, usd -> 0)
      checkStateAndCalls(bob,   Set(Waves, eth), callsCount = 1, expectedBalance = Waves -> 300.waves, eth -> 5.eth)
      checkStateAndCalls(bob,   Set(Waves, btc), callsCount = 2, expectedBalance = Waves -> 300.waves, btc -> 0)
      // format: on

      Seq(ad, sba).foreach(_ ! PoisonPill)
    }

    "handle gRPC calls exceptions" in {

      val spendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]] = { (address, assets) =>
        if (address == alice)
          Future.successful { Map(Waves -> 100.waves, usd -> 500.usd).withDefaultValue(0L).filterKeys(assets) } else
          Future.failed[Map[Asset, Long]] { WavesNodeConnectionLostException("ain't my bitch", new Exception()) }
      }

      val sba: ActorRef = system.actorOf(Props(new SpendableBalancesActor(spendableBalances, allAssetsSpendableBalances, ad)))

      def askSpendableBalance(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
        sba
          .ask(SpendableBalancesActor.Query.GetState(address, assets))(5.seconds)
          .mapTo[SpendableBalancesActor.Reply.GetState]
          .map(_.state)

      def await[T](awaitable: Future[T]): T = Await.result(awaitable, 5.second)

      await { askSpendableBalance(alice, Set(Waves, usd)) } should matchTo { Map(Waves -> 100.waves, usd -> 500.usd) }
      a[WavesNodeConnectionLostException] should be thrownBy await { askSpendableBalance(bob, Set(Waves, usd)) }
    }
  }
}
