package com.wavesplatform.dex.actors.events

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.actors.Generators
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.model.ExchangeTransactionCreator
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.charset.StandardCharsets

class OrderEventsCoordinatorActorStateSpec
    extends AnyFreeSpecLike
    with Generators
    with DiffMatcher
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  private val txCreator = new ExchangeTransactionCreator(
    matcherPrivateKey = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8))),
    exchangeTxBaseFee = defaultWavesFee,
    hasMatcherAccountScript = false,
    hasAssetScript = _ => false
  )

  private val stateWithAddressKpsGen: Gen[(List[KeyPair], OrderEventsCoordinatorActorState)] = for {
    pendingAddressesSize <- Gen.choose(0, 3)
    keyPairs <- Gen.listOfN(pendingAddressesSize, keyPairGen)
    pendingAddresses <- Gen.listOfN(pendingAddressesSize, pendingAddressGen())
    knownOnNodeCache <- knownOnNodeCacheGen
  } yield {
    val addresses = keyPairs.map(_.toAddress).zip(pendingAddresses).toMap
    val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
      case (txId, PendingTransactionType.KnownOnNode) => txId
    }
    (keyPairs, OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_)._1)))
  }

  private val stateGen: Gen[OrderEventsCoordinatorActorState] = stateWithAddressKpsGen.map(_._2)

  "OrderEventsCoordinatorActorState" - {
    "withBalanceUpdates" - {
      val testGen = for {
        state <- stateGen
        knownAddresses = state.addresses.keySet
        balancesSize <- Gen.choose(0, 3)
        balances <- {
          val unknownAddressesGen = addressGen.filterNot(knownAddresses.contains)
          Gen.mapOfN(
            balancesSize,
            Gen.zip(
              if (knownAddresses.isEmpty) unknownAddressesGen
              else Gen.oneOf(
                Gen.oneOf(knownAddresses),
                unknownAddressesGen
              ),
              balancesGen
            )
          )
        }
      } yield (state, knownAddresses, balances)

      "passes updates for not tracked addresses" in forAll(testGen) {
        case (state, knownAddresses, balances) =>
          val (_, passedBalances) = state.withBalanceUpdates(balances)
          passedBalances.keySet should matchTo(balances.keySet -- knownAddresses -- balances.collect { case (a, x) if x.isEmpty => a })
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
      val testGen = for {
        (addressKps, state) <- stateWithAddressKpsGen
        kp <- {
          val randomAddressGen = keyPairGen.filterNot(x => state.addresses.contains(x.toAddress))
          if (state.addresses.isEmpty) randomAddressGen
          else Gen.oneOf(
            Gen.oneOf(addressKps),
            randomAddressGen
          )
        }
        event <- canceledEventGen(senderGen = Gen.const(kp))
      } yield (state, event.acceptedOrder.order.sender.toAddress, event)

      "passes updates for not tracked addresses, holds updates for tracked addresses" in forAll(testGen) {
        case (state, eventAddress, event) =>
          val (_, eventOpt) = state.withPendingCancel(event)
          if (state.addresses.contains(eventAddress)) eventOpt shouldBe empty
          else eventOpt shouldNot be(empty)
      }

      "tracked information contains fresh updates if an address is tracked otherwise it remains" in forAll(testGen) {
        case (state, eventAddress, event) =>
          val (updatedState, _) = state.withPendingCancel(event)
          if (state.addresses.contains(eventAddress)) updatedState.addresses(eventAddress).events.last shouldBe event
          else updatedState should matchTo(state)
      }
    }

    "withKnownOnMatcher when the event is" - {
      "unknown" - {
        "updates the state of not resolved addresses" in {
          val testGen = for {
            (knownAddressKps, state) <- stateWithAddressKpsGen
            txId <- {
              val knownTxIds = state.addresses.values.flatMap(_.pendingTxs.keys).toSet
              txIdGen.filterNot(knownTxIds.contains) // Filter out impossible cases
            }
            event <- {
              val allKps = knownAddressKps.map(x => x.toAddress -> x).toMap

              // Collect addresses, which can't be resolved
              val kps = state.addresses.collect {
                case (address, x) if x.pendingTxs.values.exists(_ == PendingTransactionType.KnownOnMatcher) => allKps(address)
              }

              val kpGen = if (kps.isEmpty) keyPairGen else Gen.oneOf(keyPairGen, Gen.oneOf(kps))
              executedEventGen(counterGen = kpGen, submitterGen = kpGen)
            }
          } yield (state, txId, event)

          forAll(testGen) {
            case (state, txId, event) =>
              val (updatedState, resolved) = state.withKnownOnMatcher(txId, event)
              resolved shouldBe empty
              event.traders.foreach { address =>
                val pendingAddress = updatedState.addresses(address)
                pendingAddress.pendingTxs.keySet should contain(txId)
                pendingAddress.events.last shouldBe event
              }
          }
        }
      }

      "known" - {
        val testGen = for {
          pendingAddressesSize <- Gen.choose(1, 3)
          keyPairs <- Gen.listOfN(pendingAddressesSize, keyPairGen)
          resolvedSize <- Gen.choose(1, math.min(pendingAddressesSize, 2))
          (resolvedKp, notResolvedKp) = keyPairs.splitAt(resolvedSize)
          event <- resolvedKp match {
            case a :: Nil => executedEventGen(a, keyPairGen)
            case a :: b :: Nil => executedEventGen(a, b)
            case _ => throw new RuntimeException(s"Unexpected $resolvedKp")
          }
          tx = txCreator.createTransaction(event).explicitGet()
          resolved <- Gen.listOfN(
            resolvedKp.size,
            pendingAddressWithTxsGen(Gen.const(Map(tx.id() -> PendingTransactionType.KnownOnNode)))
          )
          notResolved <- Gen.listOfN(pendingAddressesSize - resolvedKp.size, pendingAddressGen())
          knownOnNodeCache <- {
            val resolvedTxIds = resolved.flatMap(_.pendingTxs.keys).toSet
            knownOnNodeCacheGen.filterNot(xs => resolvedTxIds.exists(xs.contains))
          }
        } yield {
          val resolvedAddresses = resolvedKp.map(_.toAddress).toSet
          val addresses = resolvedAddresses.zip(resolved).toMap ++ notResolvedKp.map(_.toAddress).zip(notResolved)

          val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
            case (txId, PendingTransactionType.KnownOnNode) => txId
          }

          (
            OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_)._1)),
            tx.id(),
            event,
            resolvedAddresses,
            notResolvedKp.map(_.toAddress)
          )
        }

        "resolves traders' addresses" in forAll(testGen) {
          case (state, txId, event, expectedResolvedAddresses, _) =>
            val (_, resolved) = state.withKnownOnMatcher(txId, event)
            val actualResolvedAddresses = resolved.keySet
            actualResolvedAddresses should matchTo(expectedResolvedAddresses)
            actualResolvedAddresses.foreach(address => event.traders should contain(address))
        }

        "passes holt balances and events of traders" in forAll(testGen) {
          case (state, txId, event, _, _) =>
            val (_, resolved) = state.withKnownOnMatcher(txId, event)
            resolved.foreach { case (address, updated) =>
              state.addresses.get(address) match {
                case None => fail(s"Resolved an address which wasn't in the state: $address")
                case Some(orig) =>
                  withClue("No unexpected balance changes") {
                    updated.stashedBalance should matchTo(orig.stashedBalance)
                  }
                  updated.pendingTxs shouldBe empty
                  updated.events should matchTo(orig.events.enqueue(event))
              }
            }
        }

        "removes traders from tracking" in forAll(testGen) {
          case (state, txId, event, expectedResolvedAddresses, _) =>
            val (updatedState, _) = state.withKnownOnMatcher(txId, event)
            expectedResolvedAddresses.foreach { address =>
              updatedState.addresses.get(address) shouldBe empty
            }
        }

        "not resolved traders' remain" in forAll(testGen) {
          case (state, txId, event, _, expectedNotResolvedAddresses) =>
            val (updatedState, resolved) = state.withKnownOnMatcher(txId, event)
            expectedNotResolvedAddresses.foreach { address =>
              resolved.get(address) shouldBe empty
              updatedState.addresses.get(address) shouldNot be(empty)
            }
        }
      }
    }

    "withKnownOnNode when the transaction is" - {
      val testGen = for {
        (knownAddressKps, state) <- stateWithAddressKpsGen
        knownTxIds = state.addresses.values.flatMap(_.pendingTxs.keys).toSet
        tx <- exchangeTransactionGen.filter { tx =>
          !knownTxIds.contains(tx.id()) &&
          tx.traders.intersect(knownAddressKps.map(_.toAddress).toSet).isEmpty // traders are unknown
        }
        balancesSize <- Gen.choose(0, 3)
        knownAddresses = knownAddressKps.map(_.toAddress).toSet ++ tx.traders
        balances <- Gen.mapOfN(
          balancesSize,
          Gen.zip(
            Gen.oneOf(Gen.oneOf(knownAddresses), addressGen),
            balancesGen
          )
        )
      } yield (state, knownAddresses, balances.keySet -- knownAddresses, tx, balances)

      "unknown" - {
        "doesn't resolve addresses" in forAll(testGen) {
          case (state, _, _, tx, balanceUpdates) =>
            val txId = tx.id()
            val (_, _, resolved) = state.withKnownOnNode(tx.traders, txId, balanceUpdates)
            resolved shouldBe empty
        }

        "tracked addresses' states are updated" in forAll(testGen) {
          case (state, _, _, tx, balanceUpdates) =>
            val txId = tx.id()
            val (updatedState, _, _) = state.withKnownOnNode(tx.traders, txId, balanceUpdates)
            tx.traders.foreach { address =>
              val pendingAddress = updatedState.addresses(address)
              pendingAddress.pendingTxs.keySet should contain(txId)
            }
        }

        "holds balance changes for tracked addresses" in forAll(testGen) {
          case (state, knownAddresses, _, tx, balanceUpdates) =>
            val (updatedState, passUpdates, _) = state.withKnownOnNode(tx.traders, tx.id(), balanceUpdates)

            val passUpdatesForKnown = passUpdates.view.filterKeys(knownAddresses.contains).toMap
            withClue(s"known: ${knownAddresses.mkString(", ")}")(passUpdatesForKnown shouldBe empty)

            // The latest balance changes applied
            (knownAddresses -- tx.traders).foreach { address =>
              balanceUpdates.get(address).foreach { update =>
                val pendingAddress = updatedState.addresses(address)
                val actual = pendingAddress.stashedBalance.view.filterKeys(update.contains).toMap
                actual should matchTo(update)
              }
            }
        }

        "passes balance changes for not tracked addresses" in forAll(testGen) {
          case (state, _, unknownAddresses, tx, balanceUpdates) =>
            val (_, passUpdates, _) = state.withKnownOnNode(tx.traders, tx.id(), balanceUpdates)
            val expected = balanceUpdates.filter {
              case (address, xs) => xs.nonEmpty && unknownAddresses.contains(address)
            }

            passUpdates should matchTo(expected)
        }
      }

      "known" - {
        val testGen = for {
          pendingAddressesSize <- Gen.choose(1, 3)
          keyPairs <- Gen.listOfN(pendingAddressesSize, keyPairGen)
          resolvedSize <- Gen.choose(1, math.min(pendingAddressesSize, 2))
          (resolvedKp, notResolvedKp) = keyPairs.splitAt(resolvedSize)
          tx <- resolvedKp match {
            case a :: Nil => exchangeTransactionBuyerSellerGen(a, keyPairGen)
            case a :: b :: Nil => exchangeTransactionBuyerSellerGen(a, b)
            case _ => throw new RuntimeException(s"Unexpected $resolvedKp")
          }
          resolved <- Gen.listOfN(
            resolvedKp.size,
            pendingAddressWithTxsGen(Gen.const(Map(tx.id() -> PendingTransactionType.KnownOnMatcher))) // KnownOnNode
          )
          notResolved <- Gen.listOfN(pendingAddressesSize - resolvedKp.size, pendingAddressGen())
          trackedAddresses = keyPairs.map(_.toAddress)
          notTrackedAddresses <- Gen.choose(0, 2).flatMap { n =>
            Gen.listOfN(n, addressGen.filterNot(trackedAddresses.contains))
          }
          balanceUpdates <- {
            for {
              addresses <- Gen.someOf(trackedAddresses ::: notTrackedAddresses)
              balances <- Gen.listOfN(addresses.size, balancesGen)
            } yield addresses.zip(balances).toMap
          }
          knownOnNodeCache <- {
            val resolvedTxIds = resolved.flatMap(_.pendingTxs.keys).toSet
            knownOnNodeCacheGen.filterNot(xs => resolvedTxIds.exists(xs.contains))
          }
        } yield {
          val resolvedAddresses = resolvedKp.map(_.toAddress).toSet
          val addresses = resolvedAddresses.zip(resolved).toMap ++ notResolvedKp.map(_.toAddress).zip(notResolved)

          val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
            case (txId, PendingTransactionType.KnownOnNode) => txId
          }

          KnownOnNodeExpectedItem(
            state = OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_)._1)),
            tx = tx,
            balanceUpdates = balanceUpdates,
            resolvedAddresses = resolvedAddresses,
            notResolvedAddresses = notResolvedKp.map(_.toAddress).toSet,
            notTrackedAddresses = notTrackedAddresses.toSet
          )
        }

        "resolves traders' addresses" in forAll(testGen) { t =>
          val (_, _, resolved) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)
          val actualResolvedAddresses = resolved.keySet
          actualResolvedAddresses should matchTo(t.resolvedAddresses)
          actualResolvedAddresses.foreach(address => t.tx.traders should contain(address))
        }

        "passes holt balances and events of resolved traders" in forAll(testGen) { t =>
          val (_, _, resolved) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)
          resolved.foreach { case (address, updated) =>
            t.state.addresses.get(address) match {
              case None => fail(s"Resolved an address which wasn't in the state: $address")
              case Some(orig) =>
                withClue("No unexpected balance changes") {
                  updated.stashedBalance should matchTo(orig.stashedBalance ++ t.balanceUpdates.getOrElse(address, Map.empty))
                }
                updated.pendingTxs shouldBe empty
                updated.events should matchTo(t.state.addresses(address).events)
            }
          }
        }

        "removes traders from tracking" in forAll(testGen) { t =>
          val (updatedState, _, _) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)
          t.resolvedAddresses.foreach { address =>
            updatedState.addresses.get(address) shouldBe empty
          }
        }

        "not resolved traders' remain" in forAll(testGen) { t =>
          val (updatedState, _, resolved) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)
          t.notResolvedAddresses.foreach { address =>
            resolved.get(address) shouldBe empty
            updatedState.addresses.get(address) shouldNot be(empty)
          }
        }

        "passes balances for not tracked addresses" in forAll(testGen) { t =>
          val (_, passedBalances, _) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)
          val expectedPassedBalances = t.balanceUpdates.filter {
            case (address, xs) => xs.nonEmpty && t.notTrackedAddresses.contains(address)
          }
          passedBalances should matchTo(expectedPassedBalances)
        }

        "if applied twice, affects only balances" in forAll(testGen) { t =>
          val (updatedState1, _, _) = t.state.withKnownOnNode(t.tx.traders, t.tx.id(), t.balanceUpdates)

          val balanceUpdates2 = t.balanceUpdates.view.mapValues(_.view.mapValues(_ + 1).toMap).toMap

          val (updatedState2A, passedA, _) = updatedState1.withKnownOnNode(t.tx.traders, t.tx.id(), balanceUpdates2)
          val (updatedState2B, passedB) = updatedState1.withBalanceUpdates(balanceUpdates2)

          updatedState2A should matchTo(updatedState2B)
          passedA should matchTo(passedB)
        }
      }
    }
  }

  private case class KnownOnNodeExpectedItem(
    state: OrderEventsCoordinatorActorState,
    tx: ExchangeTransaction,
    balanceUpdates: AddressAssets,
    resolvedAddresses: Set[Address],
    notResolvedAddresses: Set[Address],
    notTrackedAddresses: Set[Address]
  )

}
