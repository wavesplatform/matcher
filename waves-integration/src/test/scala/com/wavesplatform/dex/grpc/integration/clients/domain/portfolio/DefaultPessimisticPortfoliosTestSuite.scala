package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long._
import cats.instances.map._
import cats.syntax.foldable._

class DefaultPessimisticPortfoliosTestSuite extends PessimisticPortfoliosTestSuiteBase {

  "DefaultPessimisticPortfolios" - defaultBehaviorTests()

  override def mkPessimisticPortfolios(initialTxs: List[PessimisticTransaction]) = new DefaultPessimisticPortfolios(
    initialTxs.foldMap(_.pessimisticPortfolio),
    initialTxs.map(tx => tx.txId -> tx.pessimisticPortfolio).toMap
  )

}
