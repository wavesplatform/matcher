package com.wavesplatform.dex.model

import cats.data.NonEmptyList
import com.wavesplatform.dex.settings.DenormalizedMatchingRule
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DenormalizedMatchingRulesSpecification extends AnyPropSpec with PropertyChecks with Matchers with MatcherSpecBase with NoShrink {
  property("skipOutdated: rules.head.startOffset <= currentOffset < rules(1).startOffset") {
    val g = for {
      currOffset <- currOffsetGen
      rules <- rulesChainGen(5)
    } yield (currOffset, rules)

    forAll(g) {
      case (currOffset, rules) =>
        val updatedRules = DenormalizedMatchingRule.skipOutdated(currOffset, rules)
        updatedRules.toList match {
          case first :: Nil =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
          case first :: second :: _ =>
            withClue(s"first.startOffset=${first.startOffset}, currOffset=$currOffset") {
              first.startOffset shouldBe <=(currOffset)
            }
            withClue(s"currOffset=$currOffset, second.startOffset=${second.startOffset}") {
              currOffset shouldBe <(second.startOffset)
            }
          case xs => throw new IllegalStateException(s"$xs")
        }
    }
  }

  private val currOffsetGen = Gen.choose(0L, Long.MaxValue)

  private def nextRulesGen(prevRules: DenormalizedMatchingRule): Gen[Option[DenormalizedMatchingRule]] =
    if (prevRules.startOffset == Long.MaxValue) Gen.const(None)
    else
      for {
        startOffset <- Gen.choose(prevRules.startOffset + 1, Long.MaxValue)
        tickSize <- Gen.choose(1, Double.MaxValue)
      } yield Some(DenormalizedMatchingRule(startOffset, tickSize))

  private val firstRuleGen: Gen[DenormalizedMatchingRule] = Gen.choose(1, Double.MaxValue).map(DenormalizedMatchingRule(0L, _))

  private def rulesChainGen(maxNumber: Int): Gen[NonEmptyList[DenormalizedMatchingRule]] = {
    def loop(rest: Int, acc: Gen[NonEmptyList[DenormalizedMatchingRule]]): Gen[NonEmptyList[DenormalizedMatchingRule]] =
      if (rest == 0) acc
      else
        for {
          xs <- acc
          x <- nextRulesGen(xs.head)
          r <- x.fold(Gen.const(xs))(x => loop(rest - 1, Gen.const(x :: xs)))
        } yield r

    Gen.lzy(loop(maxNumber, firstRuleGen.map(NonEmptyList.one)).map(_.reverse))
  }

}
