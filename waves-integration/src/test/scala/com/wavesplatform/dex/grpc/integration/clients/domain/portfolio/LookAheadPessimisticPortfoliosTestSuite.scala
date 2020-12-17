package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long._
import cats.instances.map._
import cats.syntax.foldable._
import org.scalacheck.Gen

class LookAheadPessimisticPortfoliosTestSuite extends PessimisticPortfoliosTestSuiteBase {

  "LookAheadPessimisticPortfolios" - {
    "default behavior" - defaultBehaviorTests()

    "forged transactions" - {
      "replaceWith" - {
        val testGen = for {
          origTxs <- Gen.listOf(pessimisticTransactionGen)
          newTxs <- Gen.listOf(pessimisticTransactionGen)
          unknownTxIds <- Gen.listOf(pbTxIdGen.suchThat { txId =>
            !newTxs.exists(_.txId == txId) && !origTxs.exists(_.txId == txId)
          })
          maxForgedTransactions <- Gen.choose(0, unknownTxIds.size + 1)
        } yield (mkPessimisticPortfolios(origTxs, maxForgedTransactions), newTxs, unknownTxIds)

        "caches cleared" in forAll(testGen) { case (pp, newTxs, unknownTxIds) =>
          pp.processForged(unknownTxIds)._2 should matchTo(unknownTxIds) // Add to the cache, see below
          pp.replaceWith(newTxs)
          pp.processForged(unknownTxIds)._2 should matchTo(unknownTxIds)
        }
      }

      "unknown forged transactions" - {
        val testGen = Gen.zip(pessimisticPortfoliosGen, pessimisticTransactionGen)

        "are stored in a cache and observed in processForged" in forAll(testGen) { case (pp, unknownTx) =>
          val expectedUnknownTxIds = List(unknownTx.txId)

          val (_, actualUnknownTxIds) = pp.processForged(expectedUnknownTxIds)

          actualUnknownTxIds should matchTo(expectedUnknownTxIds)
        }

        "doesn't affect getAggregated" in forAll(testGen) { case (pp, unknownTx) =>
          val before = getState(pp)

          pp.processForged(List(unknownTx.txId))

          getState(pp) should matchTo(before)
        }

        "don't count in addPending" in forAll(testGen) { case (pp, unknownTx) =>
          val before = getState(pp)

          pp.processForged(List(unknownTx.txId))
          pp.addPending(List(unknownTx))

          getState(pp) should matchTo(before)
        }

        "are limited by maxForgedTransactions" in forAll(testGen, pessimisticTransactionGen) { case ((pp, unknownTx1), unknownTx2) =>
          val before = getState(pp)

          // unknownTx1 is removed from the cache, because maxForgedTransactions = 1
          pp.processForged(List(unknownTx1.txId, unknownTx2.txId))
          pp.addPending(List(unknownTx1, unknownTx2))

          // so only unknownTx1 affects the state
          getState(pp) should matchTo(combine(before, unknownTx1.pessimisticPortfolio))
        }
      }
    }
  }

  override def mkPessimisticPortfolios(initialTxs: List[PessimisticTransaction]) = mkPessimisticPortfolios(initialTxs, 1)

  private def mkPessimisticPortfolios(initialTxs: List[PessimisticTransaction], maxForgedTransactions: Int) = new LookAheadPessimisticPortfolios(
    new DefaultPessimisticPortfolios(
      initialTxs.foldMap(_.pessimisticPortfolio),
      initialTxs.map(tx => tx.txId -> tx.pessimisticPortfolio).toMap
    ),
    maxForgedTransactions = maxForgedTransactions
  )

}
