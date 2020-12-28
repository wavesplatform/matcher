package com.wavesplatform.dex.actors.events

import com.softwaremill.diffx.DiffxSupport
import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.account.KeyPair.toPublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.gen.{byteArrayGen, bytes32gen}
import com.wavesplatform.dex.model.Events.OrderCanceledReason
import com.wavesplatform.dex.model.{Events, LimitOrder}
import com.wavesplatform.dex.test.WavesEntitiesGen
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.Queue

class OrderEventsCoordinatorActorStateSpec
    extends AnyFreeSpecLike
    with WavesEntitiesGen
    with DiffMatcher
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  // TODO properties:
  // 1. KnownOnNode in cache + KnownOnMatcher
  // 2. PendingAddress non empty list of txId

  private val definedAssets: List[Asset] = Asset.Waves :: Gen.listOfN(2, assetGen).sample.get

  private val definedAssetsGen: Gen[Asset] = Gen.oneOf(definedAssets)

  private val addressGen: Gen[Address] = keyPairGen.map(_.toAddress)

  private val txIdGen: Gen[ExchangeTransaction.Id] = bytes32gen.map(ByteStr(_))

  private val pendingTxTypeGen: Gen[PendingTransactionType] = Gen.oneOf(PendingTransactionType.KnownOnMatcher, PendingTransactionType.KnownOnNode)

  private val balancesGen: Gen[Map[Asset, Long]] = Gen.mapOf(Gen.zip(definedAssetsGen, Gen.choose(0, 10L)))

  private val executedEventGen: Gen[Events.OrderExecuted] =
    for {
      (submitted, _) <- orderAndSenderGen(sideGen = Gen.const(OrderType.BUY), matcherGen = toPublicKey(matcher))
      (counter, _) <- orderAndSenderGen(sideGen = Gen.const(OrderType.SELL), matcherGen = toPublicKey(matcher))
    } yield Events.OrderExecuted(
      submitted = LimitOrder(submitted),
      counter = LimitOrder(counter),
      timestamp = submitted.timestamp,
      counterExecutedFee = counter.matcherFee,
      submittedExecutedFee = submitted.matcherFee
    )

  private val canceledEventGen: Gen[Events.OrderCanceled] =
    for {
      (order, _) <- orderAndSenderGen(matcherGen = toPublicKey(matcher))
    } yield Events.OrderCanceled(
      acceptedOrder = LimitOrder(order),
      reason = OrderCanceledReason.BecameInvalid,
      timestamp = order.timestamp
    )

  private val eventGen: Gen[Events.Event] = Gen.oneOf(executedEventGen, canceledEventGen)

  private val pendingAddressGen: Gen[PendingAddress] =
    for {
      pendingTxs <- Gen.nonEmptyMap(Gen.zip(txIdGen, pendingTxTypeGen))
      balances <- balancesGen
      events <- Gen.containerOf[Queue, Events.Event](eventGen)
    } yield PendingAddress(
      pendingTxs = pendingTxs,
      stashedBalance = balances,
      events = events
    )

  private val knownOnNodeCacheGen: Gen[FifoSet[ExchangeTransaction.Id]] = Gen.listOf(txIdGen).map { xs =>
    xs.foldLeft(FifoSet.limited[ExchangeTransaction.Id](100))(_.append(_))
  }

  private val stateGen: Gen[OrderEventsCoordinatorActorState] = for {
    addresses <- Gen.mapOf(Gen.zip(addressGen, pendingAddressGen))
    knownOnNodeCache <- knownOnNodeCacheGen
  } yield {
    val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
      case (txId, PendingTransactionType.KnownOnNode) => txId
    }
    OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_)))
  }

  "OrderEventsCoordinatorActorState" - {
    "withBalanceUpdates" - {
      val testGen = for {
        state <- stateGen
        knownAddresses = state.addresses.keySet
        balances <- {
          val unknownAddressesGen = addressGen.filterNot(knownAddresses.contains)
          Gen.mapOf(Gen.zip(
            if (knownAddresses.isEmpty) unknownAddressesGen
            else Gen.oneOf(
              Gen.oneOf(knownAddresses),
              unknownAddressesGen
            ),
            balancesGen
          ))
        }
      } yield (state, knownAddresses, balances)

      "passes updates for not tracked addresses" in forAll(testGen) {
        case (state, knownAddresses, balances) =>
          val (_, passedBalances) = state.withBalanceUpdates(balances)
          passedBalances.keySet should matchTo(balances.keySet -- knownAddresses)
      }

      "tracked information contains fresh updates if an address is tracked" in forAll(testGen) {
        case (state, _, balances) =>
          val (updatedState, _) = state.withBalanceUpdates(balances)

          val expected = balances.view.filterKeys(updatedState.addresses.contains).toMap

          val actual = updatedState.addresses.view.filterKeys(balances.contains)
            .map { case (address, x) => address -> x.stashedBalance.view.filterKeys(expected(address).contains).toMap }
            .toMap

          actual should matchTo(expected)
      }

      "tracked information is not changed if an address is not tracked" in {
        val testGen = for {
          state <- stateGen
          balances <- {
            val knownAddresses = state.addresses.keySet
            Gen.mapOf(Gen.zip(
              addressGen.filterNot(knownAddresses.contains),
              balancesGen
            ))
          }
        } yield (state, state.withBalanceUpdates(balances)._1)

        forAll(testGen) { case (orig, updated) =>
          orig should matchTo(updated)
        }
      }
    }

    "withPendingCancel" - {
      "passes updates for not tracked addresses, holds updates for tracked addresses" in {}
      "tracked information contains fresh updates if an address is tracked" in {}
      "tracked information is not changed if an address is not tracked" in {}
    }

    "withExecuted" - {
      "for not tracked addresses - passes updates" in {}

      "for tracked addresses" - {
        "passes updates" in {}
        "updates a tracked address information" in {}
      }

      "tracked and not tracked case" in {}
    }
  }
}
