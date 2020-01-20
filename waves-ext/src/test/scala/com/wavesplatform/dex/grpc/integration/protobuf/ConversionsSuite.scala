package com.wavesplatform.dex.grpc.integration.protobuf

import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.ToVanillaConversions._
import com.wavesplatform.dex.{NoShrink, TransactionGen}
import com.wavesplatform.dex.gen.orderGen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ConversionsSuite extends AnyFreeSpecLike with TableDrivenPropertyChecks with Matchers with TransactionGen with NoShrink {
  "order" in forAll(orderGen) { order =>
    order.toPB.toVanilla shouldBe order
  }

  "exchangeTx" in forAll(exchangeTransactionGen) { tx =>
    tx.toPB.toVanilla.right.get shouldBe tx
  }
}
