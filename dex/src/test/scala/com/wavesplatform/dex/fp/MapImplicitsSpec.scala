package com.wavesplatform.dex.fp

import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group._
import com.wavesplatform.dex.NoShrink
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MapImplicitsSpec extends AnyPropSpec with PropertyChecks with Matchers with NoShrink {

  private val mapGen = Gen.mapOf[Int, Long] {
    for {
      k <- Arbitrary.arbInt.arbitrary
      v <- Arbitrary.arbLong.arbitrary
    } yield (k, v)
  }

  property("cleaningGroup.combine - returns a map without empty values") {
    import MapImplicits.cleaningGroup
    forAll(mapGen, mapGen) {
      case (a, b) =>
        val r = (a |+| b).filter { case (_, v) => v == 0 }
        r shouldBe empty
    }
  }
}
