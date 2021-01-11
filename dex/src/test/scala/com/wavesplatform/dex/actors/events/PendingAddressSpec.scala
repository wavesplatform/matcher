package com.wavesplatform.dex.actors.events

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.fp.MapImplicits.MapOps
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.OrderExecuted
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.Queue

class PendingAddressSpec extends AnyFreeSpecLike with Generators with DiffMatcher with Matchers with ScalaCheckPropertyChecks with NoShrink {

  "PendingAddress" - {
    "withUpdatedBalances appends updated balances" in forAll(nonEmptyStateGen, balancesGen) { (state, balanceUpdates) =>
      val updated = state.withUpdatedBalances(balanceUpdates)
      updated.stashedBalance.filterByMap(balanceUpdates) shouldBe balanceUpdates
    }

    "withEvent appends events" in forAll(nonEmptyStateGen, canceledEventGen()) { (state, event) =>
      val updated = state.withEvent(event)
      updated.events.last shouldBe event
    }

    "withKnownOnNode if the transaction is" - {
      "known on node" - {
        val testGen = for {
          (state, knownOnNodeTxId) <- autoStateGen(PendingTransactionType.KnownOnNode)
          balanceUpdates <- balancesGen
        } yield (state, knownOnNodeTxId, balanceUpdates)

        def test(f: (PendingAddress, Map[Asset, Long]) => Unit): Unit = forAll(testGen) {
          case (state, knownOnNodeTxId, balanceUpdates) => f(state.withKnownOnNode(knownOnNodeTxId, balanceUpdates), balanceUpdates)
        }

        "doesn't resolve" in test { case (updated, _) =>
          updated.isResolved shouldBe false
        }

        "updates the balance" in test { case (updated, balanceUpdates) =>
          updated.stashedBalance.filterByMap(balanceUpdates) shouldBe balanceUpdates
        }
      }

      "known on matcher" - {
        val testGen = for {
          (state, knownOnMatcherTxId) <- autoStateGen(PendingTransactionType.KnownOnMatcher)
          balanceUpdates <- balancesGen
        } yield (state, state.pendingTxs.size, knownOnMatcherTxId, balanceUpdates)

        def test(f: (PendingAddress, Int, Map[Asset, Long]) => Unit): Unit = forAll(testGen) {
          case (state, txsNumber, knownOnMatcherTxId, balanceUpdates) =>
            f(state.withKnownOnNode(knownOnMatcherTxId, balanceUpdates), txsNumber, balanceUpdates)
        }

        "resolves if this is the last pending transaction" in test { case (updated, txsNumber, _) =>
          if (txsNumber > 1) updated.isResolved shouldBe false
          else updated.isResolved shouldBe true
        }

        "updates the balance" in test { case (updated, _, balanceUpdates) =>
          updated.stashedBalance.filterByMap(balanceUpdates) shouldBe balanceUpdates
        }
      }

      "unknown" - {
        val testGen = for {
          state <- nonEmptyStateGen
          unknownTxId <- txIdGen.filterNot(state.pendingTxs.contains)
          balanceUpdates <- balancesGen
        } yield (state, unknownTxId, balanceUpdates)

        def test(f: (PendingAddress, Map[Asset, Long]) => Unit): Unit = forAll(testGen) {
          case (state, unknownTxId, balanceUpdates) =>
            f(state.withKnownOnNode(unknownTxId, balanceUpdates), balanceUpdates)
        }

        "holds" in test { case (updated, _) => updated.isResolved shouldBe false }

        "updates the balance" in test { case (updated, balanceUpdates) =>
          updated.stashedBalance.filterByMap(balanceUpdates) shouldBe balanceUpdates
        }
      }
    }

    "withKnownOnMatcher if the transaction is" - {
      "known on node" - {
        val testGen = for {
          (state, knownOnNodeTxId) <- autoStateGen(PendingTransactionType.KnownOnNode)
          event <- executedEventGen()
        } yield (state, state.pendingTxs.size, knownOnNodeTxId, event)

        def test(f: (PendingAddress, Int, OrderExecuted) => Unit): Unit = forAll(testGen) {
          case (state, txsNumber, knownOnNodeTxId, event) =>
            f(state.withKnownOnMatcher(knownOnNodeTxId, event), txsNumber, event)
        }

        "resolves if this is the last pending transaction" in test { case (updated, txsNumber, _) =>
          if (txsNumber > 1) updated.isResolved shouldBe false
          else updated.isResolved shouldBe true
        }

        "adds the event" in test { case (updated, _, event) => updated.events.last should matchTo[Events.Event](event) }
      }

      "known on matcher" - {
        val testGen = for {
          (state, knownOnMatcherTxId) <- autoStateGen(PendingTransactionType.KnownOnMatcher)
          event <- executedEventGen()
        } yield (state, knownOnMatcherTxId, event)

        def test(f: (PendingAddress, OrderExecuted) => Unit): Unit = forAll(testGen) {
          case (state, knownOnMatcherTxId, event) => f(state.withKnownOnMatcher(knownOnMatcherTxId, event), event)
        }

        "doesn't resolve" in test { case (updated, _) => updated.isResolved shouldBe false }
        "doesn't add the event" in test { case (updated, event) => updated.events shouldNot contain(event) }
      }

      "unknown" - {
        val testGen = for {
          state <- nonEmptyStateGen
          unknownTxId <- txIdGen.filterNot(state.pendingTxs.contains)
          event <- executedEventGen()
        } yield (state, unknownTxId, event)

        def test(f: (PendingAddress, OrderExecuted) => Unit): Unit = forAll(testGen) {
          case (state, unknownTxId, event) =>
            f(state.withKnownOnMatcher(unknownTxId, event), event)
        }

        "holds" in test { case (updated, _) => updated.isResolved shouldBe false }
        "adds the event" in test { case (updated, event) => updated.events.last should matchTo[Events.Event](event) }
      }
    }
  }

  private def pendingAddressSizedGen(
    knownOnNodeSize: Int,
    knownOnMatcherSize: Int
  ): Gen[PendingAddress] = pendingAddressSizedGen(Gen.const(knownOnNodeSize), Gen.const(knownOnMatcherSize))

  private def pendingAddressSizedGen(
    knownOnNodeSizeGen: Gen[Int],
    knownOnMatcherSizeGen: Gen[Int]
  ): Gen[PendingAddress] =
    for {
      knownOnNodeSize <- knownOnNodeSizeGen
      knownOnNode <- Gen.mapOfN(knownOnNodeSize, Gen.zip(txIdGen, Gen.const(PendingTransactionType.KnownOnNode)))

      knownOnMatcherSize <- knownOnMatcherSizeGen
      knownOnMatcher <- Gen.mapOfN(
        knownOnMatcherSize,
        Gen.zip(txIdGen.filterNot(knownOnNode.contains), Gen.const(PendingTransactionType.KnownOnMatcher))
      )

      pendingTxs = knownOnNode ++ knownOnMatcher
      balances <- balancesGen
      events <- Gen.containerOf[Queue, Events.Event](eventGen)
    } yield PendingAddress(
      pendingTxs = pendingTxs,
      stashedBalance = balances,
      events = events
    )

  private def stateWithTxGen(
    knownOnNodeSize: Range,
    knownOnMatcherSize: Range,
    select: PendingTransactionType
  ): Gen[(PendingAddress, ExchangeTransaction.Id)] = for {
    state <- pendingAddressSizedGen(
      knownOnNodeSizeGen = Gen.choose(knownOnNodeSize.head, knownOnNodeSize.last),
      knownOnMatcherSizeGen = Gen.choose(knownOnMatcherSize.head, knownOnMatcherSize.last)
    )
    knownOnNodeTxId <- Gen.oneOf(state.pendingTxs.filter(_._2 == select).keys)
  } yield (state, knownOnNodeTxId)

  // For a selected type PendingAddress contains at least 1 tx
  private def autoStateGen(select: PendingTransactionType): Gen[(PendingAddress, ExchangeTransaction.Id)] = {
    val ranges = (0 to 2, 1 to 3)
    val (knownOnNodeSize, knownOnMatcherSize) = if (select == PendingTransactionType.KnownOnMatcher) ranges else ranges.swap
    stateWithTxGen(knownOnNodeSize, knownOnMatcherSize, select)
  }

  private def nonEmptyStateGen: Gen[PendingAddress] = for {
    knownOnNodeSize <- Gen.choose(0, 2)
    knownOnMatcherSize <- Gen.choose(if (knownOnNodeSize == 0) 1 else 0, 2)
    state <- pendingAddressSizedGen(knownOnNodeSize, knownOnMatcherSize)
  } yield state

}
