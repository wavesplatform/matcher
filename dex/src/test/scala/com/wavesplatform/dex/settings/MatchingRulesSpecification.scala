package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import com.wavesplatform.dex.model.Implicits.AssetPairOps
import org.scalatest.Matchers

class MatchingRulesSpecification extends BaseSettingsSpecification with Matchers {

  def configStr(x: String): Config = configWithSettings(matchingRulesStr = x)

  "Start offsets" should "be sorted in ascending order" in {

    def matchingRulesSettings(matchingRules: String): String =
      s"""matching-rules = {
        |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
        |    $matchingRules
        |  ]
        |}
      """.stripMargin

    def matchingRule(startOffset: String): String =
      s"""
         |    {
         |      start-offset = $startOffset
         |      tick-size    = 0.002
         |    }""".stripMargin

    val startOfsettsSets = Array(
      Array("50", "50"),
      Array("50", "49"),
      Array("30", "1"),
      Array("5", "3", "1"),
      Array("4", "1", "4"),
      Array("3", "1", "6"),
      Array("5", "5", "6"),
      Array("5", "7", "7"),
      Array("10", "17", "10"),
      Array("100", "100", "100"),
      Array("50", "50", "60")
    )

    var matchingRules = ""

    for (startOffsets <- startOfsettsSets) {
      for (startOffset <- startOffsets) {
        matchingRules ++= matchingRule(startOffset)
      }
      getSettingByConfig(configStr(matchingRulesSettings(matchingRules))) should
        produce(f"Invalid setting matching-rules value: Rules should be ordered by offset, but they are: ${startOffsets.mkString(", ")}")
      matchingRules = ""
    }
  }

  it can "not be negative" in {

    def matchingRulesSettings(startOffset: String): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = $startOffset
         |      tick-size    = 0.002
         |    }
         |  ]
         |}
      """.stripMargin

    val startOfsetts = Array("-1", "-5", "-10", "-5000000000", "-1000000000000000000000000000")

    for (startOffset <- startOfsetts) {
      getSettingByConfig(configStr(matchingRulesSettings(startOffset))) should
        produce(
          s"Invalid setting matching-rules value: Invalid setting collection-entry-path.start-offset value: $startOffset (required 0 <= start offset)")
    }
  }

  it can "be zero" in {
    val zeroStartOffsetRule =
      """matching-rules = {
        |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
        |    {
        |      start-offset = 0
        |      tick-size    = 0.002
        |    },
        |    {
        |      start-offset = 500
        |      tick-size    = 0.001
        |    }
        |  ]
        |}
      """.stripMargin

    getSettingByConfig(configStr(zeroStartOffsetRule)).explicitGet().matchingRules shouldBe Map(
      AssetPair.fromString("WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get ->
        NonEmptyList[DenormalizedMatchingRule](
          DenormalizedMatchingRule(0, 0.002),
          List(
            DenormalizedMatchingRule(500L, 0.001)
          )
        )
    )
  }

  it should "be positive" in {
    val nonEmptyCorrect =
      """matching-rules = {
        |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
        |    {
        |      start-offset = 100
        |      tick-size    = 0.002
        |    },
        |    {
        |      start-offset = 500
        |      tick-size    = 0.001
        |    }
        |  ]
        |}
      """.stripMargin

    withClue("default") {
      getSettingByConfig(configStr("")).explicitGet().matchingRules shouldBe Map.empty
    }

    withClue("nonempty correct") {
      getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().matchingRules shouldBe Map(
        AssetPair.fromString("WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get ->
          NonEmptyList[DenormalizedMatchingRule](
            DenormalizedMatchingRule(100L, 0.002),
            List(
              DenormalizedMatchingRule(500L, 0.001)
            )
          )
      )
    }
  }

  "tick size" should "be positive" in {

    def matchingRulesSettings(tickSize: String): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = 100
         |      tick-size    = $tickSize
         |    }
         |  ]
         |}
      """.stripMargin

    val tickSizes = Array("0", "-1", "-5", "-10", "-5000000000", "-1000000000000000000000000000", "-0.1")

    for (tickSize <- tickSizes) {
      getSettingByConfig(configStr(matchingRulesSettings(tickSize))) should
        produce(s"Invalid setting matching-rules value: Invalid setting collection-entry-path.tick-size value: $tickSize (required 0 < tick size)")
    }

    val tickSizesDouble = Array(-0.00000001, -0.00000000000001, -12.123, -123.1231213124234234, -123123123123123.14234234234234234234)

    for (tickSize <- tickSizesDouble) {
      getSettingByConfig(configStr(matchingRulesSettings(tickSize.toString))) should
        produce(s"Invalid setting matching-rules value: Invalid setting collection-entry-path.tick-size value: $tickSize (required 0 < tick size)")
    }
  }

  it can "be positive and fractional" in {

    def matchingRulesSettings(tickSize: String): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = 100
         |      tick-size    = $tickSize
         |    }
         |  ]
         |}
      """.stripMargin

    val tickSizes = Array(1D, 5D, 10D, 5000000000D, 1000000000000000000000000000D, 0.1D, 0.00000001D, 0.00000000000001D, 12.123D,
      123.1231213124234234D, 123123123123123.14234234234234234234D)

    for (tickSize <- tickSizes) {
      getSettingByConfig(configStr(matchingRulesSettings(tickSize.toString))).explicitGet().matchingRules shouldBe Map(
        AssetPair.fromString("WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get ->
          NonEmptyList[DenormalizedMatchingRule](
            DenormalizedMatchingRule(100L, tickSize),
            List()
          )
      )
    }
  }

  "Matching rules" can "not be empty" in {
    val emptyTickSize =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |  ]
         |}
      """.stripMargin

    getSettingByConfig(configStr(emptyTickSize)) should produce("Invalid setting matching-rules value: Expected at least one element")
  }
}
