package com.wavesplatform.dex.actors

import java.util.concurrent.atomic.AtomicReference

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{Actor, ActorRef, ActorSystem, Kill, Props, Terminated}
import akka.testkit.{ImplicitSender, TestActor, TestActorRef, TestProbe}
import cats.data.NonEmptyList
import cats.implicits.catsSyntaxEitherId
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.MatcherActor.{ForceStartOrderBook, GetMarkets, MarketData, SaveSnapshot}
import com.wavesplatform.dex.actors.MatcherActorSpecification.{DeletingActor, FailAtStartActor, NothingDoActor, RecoveringActor, _}
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.{OrderBookRecovered, OrderBookSnapshotUpdateCompleted}
import com.wavesplatform.dex.actors.orderbook.OrderBookSnapshotStoreActor.{Message, Response}
import com.wavesplatform.dex.actors.orderbook.{AggregatedOrderBookActor, OrderBookActor, OrderBookSnapshotStoreActor}
import com.wavesplatform.dex.db.{AssetPairsDB, OrderBookSnapshotDB, WithDB}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.{Events, OrderBookSnapshot}
import com.wavesplatform.dex.queue.{ValidatedCommand, ValidatedCommandWithMeta}
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule}
import com.wavesplatform.dex.time.SystemTime
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

class MatcherActorSpecification
    extends MatcherSpec("MatcherActor")
    with MatcherSpecBase
    with WithDB
    with HasOecInteraction
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender
    with Eventually
    with SystemTime {

  private def assetDescription(assetId: Asset): Option[BriefAssetDescription] =
    Some(BriefAssetDescription(name = "Unknown", decimals = 8, hasScript = false))

  "MatcherActor" should {
    "return all open markets" in {
      val actor = defaultActor()
      val probe = TestProbe()

      val pair = AssetPair(randomAssetId, randomAssetId)
      val order = buy(pair, 2000L, 1)

      probe.send(actor, wrapLimitOrder(order))
      probe.send(actor, GetMarkets)

      probe.expectMsgPF() { case s @ Seq(MarketData(_, "Unknown", "Unknown", _, _, _)) =>
        s.size shouldBe 1
      }
    }

    "successfully route the first place to the new order book" in {
      val addressActor = TestProbe()
      val actor = defaultActor(addressActor = addressActor.ref)
      val probe = TestProbe()

      val pair = AssetPair(randomAssetId, randomAssetId)
      val order = buy(pair, 2000L, 1)

      probe.send(actor, wrapLimitOrder(order))
      addressActor.expectOecProcess[Events.OrderAdded]
    }

    "mark an order book as failed" when {
      "it crashes at start" in {
        val pair = AssetPair(randomAssetId, randomAssetId)
        val ob = emptyOrderBookRefs
        val actor = TestActorRef(
          new MatcherActor(
            matcherSettings,
            mkAssetPairsDB,
            doNothingOnRecovery,
            ob,
            (_, _) => Props(new FailAtStartActor),
            assetDescription,
            _.asRight
          )
        )

        val probe = TestProbe()
        probe.send(actor, wrapLimitOrder(buy(pair, 2000L, 1)))
        eventually(ob.get()(pair) shouldBe Symbol("left"))
        probe.expectNoMessage()
      }

      "it crashes during the work" in {
        val ob = emptyOrderBookRefs
        val actor = defaultActor(ob)
        val probe = TestProbe()

        val a1, a2, a3 = randomAssetId

        val pair1 = AssetPair(a1, a2)
        val order1 = buy(pair1, 2000L, 1)

        val pair2 = AssetPair(a2, a3)
        val order2 = buy(pair2, 2000L, 1)

        probe.send(actor, wrapLimitOrder(order1))
        probe.send(actor, wrapLimitOrder(order2))

        eventually {
          ob.get()(pair1) shouldBe Symbol("right")
          ob.get()(pair2) shouldBe Symbol("right")
        }

        val toKill = actor.getChild(List(OrderBookActor.name(pair1)).iterator)

        probe.watch(toKill)
        toKill.tell(Kill, actor)
        probe.expectMsgType[Terminated]

        ob.get()(pair1) shouldBe Symbol("left")
      }
    }

    "continue the work when recovery is successful" in {
      val ob = emptyOrderBookRefs
      var working = false

      val actor = system.actorOf(
        Props(
          new MatcherActor(
            matcherSettings,
            mkAssetPairsDB,
            startResult => working = startResult.isRight,
            ob,
            (_, _) => Props(new FailAtStartActor()),
            assetDescription,
            _.asRight
          )
        )
      )

      val probe = TestProbe()
      probe.send(actor, MatcherActor.GetSnapshotOffsets)
      probe.expectMsg(MatcherActor.SnapshotOffsetsResponse(Map.empty))

      eventually(timeout(2.seconds))(working shouldBe true)
    }

    "stop the work" when {
      "an order book as failed during recovery" in {
        val apdb = mkAssetPairsDB
        val obsdb = mkOrderBookSnapshotDB
        val pair = AssetPair(randomAssetId, randomAssetId)
        val ob = emptyOrderBookRefs
        var stopped = false

        matcherHadOrderBooksBefore(apdb, obsdb, pair -> 1)
        val probe = TestProbe()
        val actor = probe.watch(
          system.actorOf(
            Props(
              new MatcherActor(
                matcherSettings,
                apdb,
                startResult => stopped = startResult.isLeft,
                ob,
                (_, _) => Props(new FailAtStartActor),
                assetDescription,
                _.asRight
              )
            )
          )
        )

        probe.expectTerminated(actor)
        stopped shouldBe true
      }

      "received Shutdown during start" in {
        val apdb = mkAssetPairsDB
        val obsdb = mkOrderBookSnapshotDB
        val pair = AssetPair(randomAssetId, randomAssetId)
        val ob = emptyOrderBookRefs
        var stopped = false

        matcherHadOrderBooksBefore(apdb, obsdb, pair -> 1)
        val probe = TestProbe()
        val actor = probe.watch(
          system.actorOf(
            Props(
              new MatcherActor(
                matcherSettings,
                apdb,
                startResult => stopped = startResult.isLeft,
                ob,
                (_, _) => Props(new NothingDoActor),
                assetDescription,
                _.asRight
              )
            )
          )
        )
        probe.send(actor, MatcherActor.Shutdown)

        probe.expectTerminated(actor)
        stopped shouldBe true
      }
    }

    "delete order books" is pending
    "forward new orders to order books" is pending

    // snapshotOffset == 17
    val pair23 = AssetPair(IssuedAsset(ByteStr(Array(1))), IssuedAsset(ByteStr(Array(2)))) // key = 2-3, snapshot offset = 9: 9, 26, 43, ...
    val pair45 = AssetPair(IssuedAsset(ByteStr(Array(3))), IssuedAsset(ByteStr(Array(4)))) // key = 4-5, snapshot offset = 12: 12, 29, 46, ...

    "force an order book to create a snapshot" when {
      "it didn't do snapshots for a long time" when {
        "first time" in snapshotTest(pair23) { (matcherActor, probes) =>
          val eventSender = TestProbe()
          sendBuyOrders(eventSender, matcherActor, pair23, 0 to 9)
          probes.head.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(9)))
        }

        "later" in snapshotTest(pair23) { (matcherActor, probes) =>
          val eventSender = TestProbe()
          val probe = probes.head
          sendBuyOrders(eventSender, matcherActor, pair23, 0 to 10)
          probe.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(9)))

          sendBuyOrders(eventSender, matcherActor, pair23, 10 to 28)
          probe.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(26)))
        }

        "multiple order books" in snapshotTest(pair23, pair45) { (matcherActor, probes) =>
          val eventSender = TestProbe()
          val (probe23, probe45) = probes match {
            case List(probe23, probe45) => (probe23, probe45)
            case _ => throw new IllegalArgumentException(s"Unexpected snapshots")
          }
          sendBuyOrders(eventSender, matcherActor, pair23, 0 to 1)
          sendBuyOrders(eventSender, matcherActor, pair45, 2 to 3)

          probe23.expectNoMessage(200.millis)
          probe45.expectNoMessage(200.millis)

          sendBuyOrders(eventSender, matcherActor, pair45, 4 to 10)
          probe23.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(9)))
          probe45.expectNoMessage(200.millis)

          sendBuyOrders(eventSender, matcherActor, pair23, 11 to 14)
          probe23.expectNoMessage(200.millis)
          probe45.expectMsg(OrderBookSnapshotUpdateCompleted(pair45, Some(12)))
        }
      }

      "received a lot of messages and skipped the middle offset" in snapshotTest(pair23) { (matcherActor, probes) =>
        val eventSender = TestProbe()
        val probe = probes.head
        sendBuyOrders(eventSender, matcherActor, pair23, 0 to 30)
        probe.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(9)))

        // OrderBookSnapshotUpdated(pair23, 26) is ignored in OrderBookActor, because it's waiting for SaveSnapshotSuccess of 9 from SnapshotStore.
        probe.expectNoMessage(200.millis)

        sendBuyOrders(eventSender, matcherActor, pair23, 31 to 45)
        probe.expectMsg(OrderBookSnapshotUpdateCompleted(pair23, Some(43)))
        probe.expectNoMessage(200.millis)
      }
    }

    "create an order book" when {
      "place order - new order book" in {
        val apdb = mkAssetPairsDB
        val obsdb = mkOrderBookSnapshotDB
        val pair1 = AssetPair(randomAssetId, randomAssetId)
        val pair2 = AssetPair(randomAssetId, randomAssetId)

        matcherHadOrderBooksBefore(apdb, obsdb, pair1 -> 9)
        val ob = emptyOrderBookRefs
        val actor = defaultActor(ob, apdb = apdb, snapshotStoreActor = system.actorOf(OrderBookSnapshotStoreActor.props(obsdb)))

        val probe = TestProbe()
        probe.send(actor, MatcherActor.GetSnapshotOffsets)
        probe.expectMsg(MatcherActor.SnapshotOffsetsResponse(Map(pair1 -> Some(9L))))

        probe.send(actor, wrapLimitOrder(buy(pair2, 2000L, 1)))
        eventually {
          probe.send(actor, MatcherActor.GetSnapshotOffsets)
          probe.expectMsg(MatcherActor.SnapshotOffsetsResponse(Map(pair1 -> Some(9L), pair2 -> None)))
        }
      }

      "force request" in {
        val pair = AssetPair(randomAssetId, randomAssetId)
        val ob = emptyOrderBookRefs

        val probe = TestProbe()
        val actor = system.actorOf(
          Props(
            new MatcherActor(
              matcherSettings,
              mkAssetPairsDB,
              _ => {},
              ob,
              (pair, matcherActor) => Props(new RecoveringActor(matcherActor, pair)),
              assetDescription,
              _.asRight
            )
          )
        )

        probe.send(actor, MatcherActor.ForceStartOrderBook(pair))
        probe.expectMsg(MatcherActor.OrderBookCreated(pair))
      }

      "after delete" in {
        val apdb = mkAssetPairsDB
        val obsdb = OrderBookSnapshotDB.inMem
        val pair = AssetPair(randomAssetId, randomAssetId)

        matcherHadOrderBooksBefore(apdb, obsdb, pair -> 9L)
        val ob = emptyOrderBookRefs
        val actor = TestActorRef(
          new MatcherActor(
            matcherSettings,
            apdb,
            doNothingOnRecovery,
            ob,
            (assetPair, matcher) => Props(new DeletingActor(matcher, assetPair, Some(9L))),
            assetDescription,
            _.asRight
          )
        )

        val probe = TestProbe()
        probe.send(actor, ValidatedCommandWithMeta(10L, 0L, ValidatedCommand.DeleteOrderBook(pair)))

        withClue("Removed from snapshots rotation") {
          eventually {
            probe.send(actor, GetMarkets)
            probe.expectMsgPF() { case xs @ Seq() =>
              xs.size shouldBe 0 // To ignore annoying warnings
            }

            probe.send(actor, MatcherActor.GetSnapshotOffsets)
            probe.expectMsg(MatcherActor.SnapshotOffsetsResponse(Map.empty))
          }
        }

        withClue("Can be re-created") {
          val order1 = buy(pair, 2000L, 1)
          probe.send(actor, wrapLimitOrder(11L, order1))

          eventually {
            probe.send(actor, GetMarkets)
            probe.expectMsgPF() { case Seq(x: MarketData) =>
              x.pair shouldBe pair
            }

            probe.send(actor, MatcherActor.GetSnapshotOffsets)
            probe.expectMsg(MatcherActor.SnapshotOffsetsResponse(Map.empty))
          }
        }
      }
    }
  }

  private def sendBuyOrders(eventSender: TestProbe, actor: ActorRef, assetPair: AssetPair, indexes: Range): Unit = {
    val ts = System.currentTimeMillis()
    indexes.foreach { i =>
      eventSender.send(actor, wrapLimitOrder(i, buy(assetPair, amount = 1000L, price = 1, ts = Some(ts + i))))
    }
  }

  /**
   * @param f (MatcherActor, TestProbe) => Any
   */
  private def snapshotTest(assetPairs: AssetPair*)(f: (ActorRef, List[TestProbe]) => Any): Any = {
    val r = assetPairs.map(fakeOrderBookActor).toList
    val actor = TestActorRef(
      new MatcherActor(
        matcherSettings.copy(snapshotsInterval = 17),
        mkAssetPairsDB,
        doNothingOnRecovery,
        emptyOrderBookRefs,
        (assetPair, _) => {
          val idx = assetPairs.indexOf(assetPair)
          if (idx < 0) throw new RuntimeException(s"Can't find $assetPair in $assetPairs")
          r(idx)._1
        },
        assetDescription,
        _.asRight
      )
    )

    f(actor, r.map(_._2))
  }

  private def fakeOrderBookActor(assetPair: AssetPair): (Props, TestProbe) = {
    val probe = TestProbe()
    val props = Props(new Actor {
      private var nr = -1L

      override def receive: Receive = {
        case x: ValidatedCommandWithMeta if x.offset > nr => nr = x.offset
        case SaveSnapshot(globalNr) =>
          val event = OrderBookSnapshotUpdateCompleted(assetPair, Some(globalNr))
          context.system.scheduler.scheduleOnce(200.millis) {
            context.parent ! event
            probe.ref ! event
          }
      }
      context.parent ! OrderBookRecovered(assetPair, None)
    })

    (props, probe)
  }

  private def defaultActor(
    ob: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]] = emptyOrderBookRefs,
    apdb: AssetPairsDB = mkAssetPairsDB,
    addressActor: ActorRef = TestProbe().ref,
    snapshotStoreActor: ActorRef = emptySnapshotStoreActor
  ): TestActorRef[MatcherActor] = {
    implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

    TestActorRef(
      new MatcherActor(
        matcherSettings,
        apdb,
        doNothingOnRecovery,
        ob,
        (assetPair, matcher) =>
          OrderBookActor.props(
            OrderBookActor.Settings(AggregatedOrderBookActor.Settings(100.millis)),
            matcher,
            addressActor,
            snapshotStoreActor,
            system.toTyped.ignoreRef,
            assetPair,
            time,
            NonEmptyList.one(DenormalizedMatchingRule(0, 0.00000001)),
            _ => {},
            _ => MatchingRule.DefaultRule,
            _ => makerTakerPartialFee,
            None
          ),
        assetDescription,
        _.asRight
      )
    )
  }

  private def mkAssetPairsDB: AssetPairsDB = AssetPairsDB(db)
  private def mkOrderBookSnapshotDB: OrderBookSnapshotDB = OrderBookSnapshotDB(db)

  private def matcherHadOrderBooksBefore(apdb: AssetPairsDB, obsdb: OrderBookSnapshotDB, pairs: (AssetPair, Long)*): Unit = {
    pairs.map(_._1).foreach(apdb.add)
    pairs.foreach { case (pair, offset) => obsdb.update(pair, offset, Some(OrderBookSnapshot.empty)) }
  }

  private def doNothingOnRecovery(x: Either[String, ValidatedCommandWithMeta.Offset]): Unit = {}

  private def emptyOrderBookRefs = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private def randomAssetId: Asset = IssuedAsset(ByteStr(randomBytes()))
}

object MatcherActorSpecification {
  private class NothingDoActor extends Actor { override def receive: Receive = Actor.ignoringBehavior }

  private class RecoveringActor(owner: ActorRef, assetPair: AssetPair, startOffset: Option[Long] = None) extends Actor {
    context.system.scheduler.scheduleOnce(50.millis, owner, OrderBookRecovered(assetPair, startOffset)) // emulates recovering
    override def receive: Receive = {
      case ForceStartOrderBook(p) if p == assetPair => sender() ! MatcherActor.OrderBookCreated(assetPair)
      case _ =>
    }

  }

  private class FailAtStartActor extends Actor {
    override def receive: Receive = Actor.emptyBehavior

    override def preStart(): Unit = {
      super.preStart()
      Thread.sleep(50)
      throw new RuntimeException("I don't want to work today")
    }

  }

  private class DeletingActor(owner: ActorRef, assetPair: AssetPair, startOffset: Option[Long] = None)
      extends RecoveringActor(owner, assetPair, startOffset) {
    override def receive: Receive = handleDelete orElse super.receive
    private def handleDelete: Receive = { case ValidatedCommandWithMeta(_, _, _: ValidatedCommand.DeleteOrderBook) => context.stop(self) }
  }

  private def emptySnapshotStoreActor(implicit actorSystem: ActorSystem): ActorRef = {
    val p = TestProbe()
    p.setAutoPilot { (sender: ActorRef, msg: Any) =>
      msg match {
        case _: Message.GetSnapshot => sender ! Response.GetSnapshot(None)
        case _ => throw new IllegalArgumentException(s"Unexpected error")
      }
      TestActor.KeepRunning
    }
    p.ref
  }

}
