package com.wavesplatform.dex.grpc.integration.protobuf

import com.wavesplatform.dex.grpc.integration.protobuf.PbToWavesConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions._
import com.wavesplatform.dex.test.WavesEntitiesGen
import com.wavesplatform.dex.{NoShrink, WavesExtSuiteBase}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConversionsSuite extends WavesExtSuiteBase with ScalaCheckDrivenPropertyChecks with WavesEntitiesGen with NoShrink {
  "order" in forAll(orderAndSenderGen()) { orderAndSender =>
    val (order, _) = orderAndSender
    order.toPB.toVanilla shouldBe order
  }

  "exchangeTx" in forAll(exchangeTransactionGen) { tx =>
    tx.toPB.toVanilla.explicitGetErr() shouldBe tx
  }
}
