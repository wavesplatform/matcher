package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long._
import cats.instances.map._
import cats.syntax.foldable._

class LookAheadPessimisticPortfoliosTestSuite extends PessimisticPortfoliosTestSuiteBase {

  "LookAheadPessimisticPortfolios" - {
    "default behavior" - defaultBehaviorTests()

    "forged transactions" - {
      // DEX-1004
      "replaceWith" ignore {}

      "addPending" - {}
      "processForged" - {}
    }
  }

  override def mkPessimisticPortfolios(initialTxs: List[PessimisticTransaction]) = new LookAheadPessimisticPortfolios(
    new DefaultPessimisticPortfolios(
      initialTxs.foldMap(_.pessimisticPortfolio),
      initialTxs.map(tx => tx.txId -> tx.pessimisticPortfolio).toMap
    ),
    maxForgedTransactions = 1
  )

}
