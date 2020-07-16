package com.wavesplatform.dex.grpc.integration.protobuf

import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.grpc.integration.protobuf.PbToWavesConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.WavesToPbConversions._
import com.wavesplatform.dex.test.WavesEntitiesGen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConversionsSuite extends AnyFreeSpecLike with ScalaCheckDrivenPropertyChecks with WavesEntitiesGen with Matchers with NoShrink {
  "order" in forAll(orderAndSenderGen()) { orderAndSender =>
    val (order, _) = orderAndSender
    order.toPB.toVanilla shouldBe order
  }

  "exchangeTx" in forAll(exchangeTransactionGen) { tx =>
    tx.toPB.toVanilla.explicitGetErr() shouldBe tx
  }
}
