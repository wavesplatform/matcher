package com.wavesplatform.dex.grpc.integration.protobuf

import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.ToVanillaConversions._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{FreeSpecLike, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => PropertyChecks}

class ConversionsSuite extends FreeSpecLike with PropertyChecks with Matchers with TransactionGen with NoShrink {
  "order" in forAll(orderGen) { order =>
    order.toPB.toVanilla shouldBe order
  }

  "exchangeTx" in forAll(exchangeTransactionGen) { tx =>
    tx.toPB.toVanilla.right.get shouldBe tx
  }
}
