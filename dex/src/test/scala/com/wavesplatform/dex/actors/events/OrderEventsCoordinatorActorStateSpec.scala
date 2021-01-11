package com.wavesplatform.dex.actors.events

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.transaction.ExchangeTransactionV2
import com.wavesplatform.dex.domain.utils.EitherExt2
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OrderEventsCoordinatorActorStateSpec
    extends AnyFreeSpecLike
    with Generators
    with DiffMatcher
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  // TODO properties:
  // 1. KnownOnNode in cache + KnownOnMatcher
  // 2. PendingAddress non empty list of txId

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
    (keyPairs, OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_))))
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

    "withExecuted when the event is" - {
      "unexpected" - {
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
              val (updatedState, resolved) = state.withExecuted(txId, event)
              resolved shouldBe empty
              event.traders.foreach { address =>
                val pendingAddress = updatedState.addresses(address)
                pendingAddress.pendingTxs.keySet should contain(txId)
                pendingAddress.events.last shouldBe event
              }
          }
        }

        "for tracked addresses and resolved" - {
          val customStateGen: Gen[(List[KeyPair], OrderEventsCoordinatorActorState)] = for {
            pendingAddressesSize <- Gen.choose(1, 3)
            keyPairs <- Gen.listOfN(pendingAddressesSize, keyPairGen)
            willBeResolvedSize <- Gen.choose(1, pendingAddressesSize)
            willBeResolved <- Gen.listOfN(
              willBeResolvedSize,
              pendingAddressGen(
                pendingTxsSizeGen = Gen.const(1),
                pendingTxTypeGen = Gen.const(PendingTransactionType.KnownOnMatcher)
              )
            )
            wontResolved <- Gen.listOfN(pendingAddressesSize - willBeResolvedSize, pendingAddressGen())
            knownOnNodeCache <- {
              val resolvedTxIds = willBeResolved.flatMap(_.pendingTxs.keys).toSet
              knownOnNodeCacheGen.filterNot(xs => resolvedTxIds.exists(xs.contains))
            }
          } yield {
            val addresses = keyPairs.map(_.toAddress).zip(willBeResolved ::: wontResolved).toMap
            val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
              case (txId, PendingTransactionType.KnownOnNode) => txId
            }
            (keyPairs, OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_))))
          }

          val testGen = for {
            (knownAddressKps, state) <- customStateGen
            txId <- {
              val knownTxIds = state.addresses.values.flatMap(_.pendingTxs.keys).toSet
              txIdGen.filterNot(knownTxIds.contains) // Filter out impossible cases
            }
            event <- {
              val allKps = knownAddressKps.map(x => x.toAddress -> x).toMap

              // Collect addresses, which can't be resolved
              val kps = state.addresses.collect {
                case (address, x) if x.pendingTxs.size == 1 && x.pendingTxs.head._2 == PendingTransactionType.KnownOnMatcher => allKps(address)
              }

              val kpGen = if (kps.isEmpty) keyPairGen else Gen.oneOf(keyPairGen, Gen.oneOf(kps))
              executedEventGen(counterGen = kpGen, submitterGen = kpGen)
            }
          } yield (state, txId, event, Seq.empty[Address])

          "passes updates and removes resolved addresses from tracking" in forAll(testGen) {
            case (state, txId, event, resolvedAddresses) =>
              val (updatedState, resolved) = state.withExecuted(txId, event)
              val xs = resolved.keySet

              xs should matchTo(resolvedAddresses.toSet)
              xs.foreach { address =>
                updatedState.addresses.get(address) shouldBe empty
              }
          }
        }
      }

      "expected" ignore {}
    }

    "withKnownOnNodeTx when the transaction is" - {
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

      "unexpected" - {
        "doesn't resolve addresses" in forAll(testGen) {
          case (state, _, _, tx, balanceUpdates) =>
            val txId = tx.id()
            val (_, _, resolved) = state.withKnownOnNodeTx(tx.traders, txId, balanceUpdates)
            resolved shouldBe empty
        }

        "tracked addresses' states are updated" in forAll(testGen) {
          case (state, _, _, tx, balanceUpdates) =>
            val txId = tx.id()
            val (updatedState, _, _) = state.withKnownOnNodeTx(tx.traders, txId, balanceUpdates)
            tx.traders.foreach { address =>
              val pendingAddress = updatedState.addresses(address)
              pendingAddress.pendingTxs.keySet should contain(txId)
            }
        }

        "holds balance changes for tracked addresses" in forAll(testGen) {
          case (state, knownAddresses, _, tx, balanceUpdates) =>
            val (updatedState, passUpdates, _) = state.withKnownOnNodeTx(tx.traders, tx.id(), balanceUpdates)

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

        // TODO custom generator
        "passes balance changes for not tracked addresses" in forAll(testGen) {
          case (state, _, unknownAddresses, tx, balanceUpdates) =>
            val (_, passUpdates, _) = state.withKnownOnNodeTx(tx.traders, tx.id(), balanceUpdates)
            if (unknownAddresses.nonEmpty) {
              val passUpdatesForUnknown = passUpdates.view.filterKeys(unknownAddresses.contains).toMap
              withClue(s"unknown (${unknownAddresses.mkString(", ")}): ")(passUpdatesForUnknown shouldNot be(empty))
            }
        }
      }

      "expected" - {
        "resolves traders' addresses" ignore {}

        "passes holt balances and events of traders" ignore {}

        "removes traders from tracking" ignore {}

        "removes traders from tracking" ignore {}
      }

      "for tracked addresses and resolved" ignore {
        def txGen(buyerGen: Gen[KeyPair], sellerGen: Gen[KeyPair]): Gen[ExchangeTransactionV2] = for {
          matcher <- keyPairGen
          matcherPublicKeyGen = Gen.const(matcher.publicKey)
          timestamp <- timestampGen
          orderTimestampGen = Gen.choose(1, 1000L).map(_ + timestamp)
          (buyOrder, _) <- orderAndSenderGen(
            sideGen = Gen.const(OrderType.BUY),
            senderGen = buyerGen,
            matcherGen = matcherPublicKeyGen,
            timestampGen = orderTimestampGen
          )
          (sellOrder, _) <- orderAndSenderGen(
            sideGen = Gen.const(OrderType.SELL),
            senderGen = sellerGen,
            matcherGen = matcherPublicKeyGen,
            assetPairGen = Gen.const(buyOrder.assetPair),
            priceGen = Gen.choose(1L, buyOrder.price),
            timestampGen = orderTimestampGen
          )
        } yield {
          val amount = math.min(buyOrder.amount, sellOrder.amount)
          val price = buyOrder.price
          ExchangeTransactionV2
            .create(
              matcher = matcher,
              buyOrder = buyOrder,
              sellOrder = sellOrder,
              amount = amount,
              price = price,
              buyMatcherFee = buyOrder.matcherFee,
              sellMatcherFee = sellOrder.matcherFee,
              fee = defaultWavesFee,
              timestamp = timestamp
            )
            .explicitGet()
        }

        val customStateGen: Gen[(List[KeyPair], OrderEventsCoordinatorActorState)] = for {
          pendingAddressesSize <- Gen.choose(1, 3)
          keyPairs <- Gen.listOfN(pendingAddressesSize, keyPairGen)
          willBeResolvedSize <- Gen.choose(1, pendingAddressesSize)
          willBeResolved <- Gen.listOfN(
            willBeResolvedSize,
            pendingAddressGen(
              pendingTxsSizeGen = Gen.const(1),
              pendingTxTypeGen = Gen.const(PendingTransactionType.KnownOnMatcher)
            )
          )
          wontResolved <- Gen.listOfN(pendingAddressesSize - willBeResolvedSize, pendingAddressGen())
          knownOnNodeCache <- {
            val resolvedTxIds = willBeResolved.flatMap(_.pendingTxs.keys).toSet
            knownOnNodeCacheGen.filterNot(xs => resolvedTxIds.exists(xs.contains))
          }
        } yield {
          val addresses = keyPairs.map(_.toAddress).zip(willBeResolved ::: wontResolved).toMap
          val knownOnNodeByPending = addresses.values.flatMap(_.pendingTxs).collect {
            case (txId, PendingTransactionType.KnownOnNode) => txId
          }
          (keyPairs, OrderEventsCoordinatorActorState(addresses, knownOnNodeByPending.foldLeft(knownOnNodeCache)(_.append(_))))
        }

        val testGen = for {
          (knownAddressKps, state) <- customStateGen
          txId <- {
            val knownTxIds = state.addresses.values.flatMap(_.pendingTxs.keys).toSet
            txIdGen.filterNot(knownTxIds.contains) // Filter out impossible cases
          }
          event <- {
            val allKps = knownAddressKps.map(x => x.toAddress -> x).toMap

            // Collect addresses, which can't be resolved
            val kps = state.addresses.collect {
              case (address, x) if x.pendingTxs.size == 1 && x.pendingTxs.head._2 == PendingTransactionType.KnownOnMatcher => allKps(address)
            }

            val kpGen = if (kps.isEmpty) keyPairGen else Gen.oneOf(keyPairGen, Gen.oneOf(kps))
            executedEventGen(counterGen = kpGen, submitterGen = kpGen)
          }
        } yield (state, txId, event, Seq.empty[Address])

        "passes updates and removes resolved addresses from tracking" in forAll(testGen) {
          case (state, txId, event, resolvedAddresses) =>
            val (updatedState, resolved) = state.withExecuted(txId, event)
            val xs = resolved.keySet

            xs should matchTo(resolvedAddresses.toSet)
            xs.foreach { address =>
              updatedState.addresses.get(address) shouldBe empty
            }
        }
      }
    }
  }
}
