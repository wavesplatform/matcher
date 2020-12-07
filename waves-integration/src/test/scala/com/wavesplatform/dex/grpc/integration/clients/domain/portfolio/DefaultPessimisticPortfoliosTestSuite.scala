package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DefaultPessimisticPortfoliosTestSuite extends WavesIntegrationSuiteBase with ScalaCheckDrivenPropertyChecks with NoShrink {

  "DefaultPessimisticPortfolios" - {
    "getAggregated" - {}

    "replaceWith" - {
      // Whatever a state we have, the new state should be equal to replaceWith
    }

    "addPending" - {
      "empty doesn't affect" in {
      }
    }

    "processForged" - {}

    // TODO DEX-1013
    "removeFailed" ignore {}
  }

  private def mk: PessimisticPortfolios = new DefaultPessimisticPortfolios(new PessimisticStorage)

}
