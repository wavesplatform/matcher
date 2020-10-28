package com.wavesplatform.dex.caches

import com.wavesplatform.dex.NoShrink
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DropOldestFixedBuffer2Spec extends AnyFreeSpecLike with Matchers with PropertyChecks with NoShrink {
  private val doubleGen = Arbitrary.arbDouble.arbitrary
  private val testGen = Gen.zip(doubleGen, Gen.listOf(doubleGen))

  "DropOldestFixedBuffer2" - {
    "min" - {
      "selects the minimal value between two last" in forAll(testGen) { case (init, xs) =>
        val buff = xs.foldLeft(DropOldestFixedBuffer2(init))(_.append(_))
        val lastTwo = (init :: xs).takeRight(2)
        buff.min shouldBe lastTwo.min
      }
    }
  }
}
