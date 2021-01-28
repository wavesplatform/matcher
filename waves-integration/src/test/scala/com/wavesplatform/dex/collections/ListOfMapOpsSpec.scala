package com.wavesplatform.dex.collections

import com.wavesplatform.dex.collections.ListOps.ListOfMapsOps
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ListOfMapOpsSpec extends WavesIntegrationSuiteBase with Matchers with ScalaCheckDrivenPropertyChecks with NoShrink {
  "ListOfMapOps" - {
    "foldSkipped" - {
      "folds elements in an expected way" in {
        List(Map('a' -> 1, 'b' -> 2), Map('a' -> 4, 'd' -> 3)).foldSkipped should matchTo(Map(
          'a' -> 1,
          'b' -> 2,
          'd' -> 3
        ))
      }

      "is equivalent with a special foldRight" in {
        def slow[K, V](xs: List[Map[K, V]]): Map[K, V] = xs.foldRight(Map.empty[K, V]) { case (rs, xs) => xs ++ rs }

        val testGen = Gen.listOf(Gen.mapOf(Gen.zip(Gen.alphaChar, Gen.choose(-10, 10))))
        forAll(testGen) { xs: List[Map[Char, Int]] => xs.foldSkipped should matchTo(slow(xs)) }
      }
    }
  }
}
