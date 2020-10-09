package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Implicits.AssetPairOps
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import org.scalatest.matchers.should.Matchers

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

    def matchingRule(startOffset: Int): String =
      s"""
         |    {
         |      start-offset = $startOffset
         |      tick-size    = 0.002
         |    }""".stripMargin

    Seq(
      Seq(50, 50),
      Seq(50, 49),
      Seq(30, 1),
      Seq(5, 3, 1),
      Seq(4, 1, 4),
      Seq(3, 1, 6),
      Seq(5, 5, 6),
      Seq(5, 7, 7),
      Seq(10, 17, 10),
      Seq(100, 100, 100),
      Seq(50, 50, 60)
    ).foreach { startOffsets =>
      val matchingRules = startOffsets.map(matchingRule).mkString("\n")
      getSettingByConfig(configStr(matchingRulesSettings(matchingRules))) should produce(
        s"waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.+\n.+but they are: ${startOffsets.mkString(", ")}".r
      )
    }
  }

  it can "not be negative" in {

    def matchingRulesSettings(startOffset: Long): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = $startOffset
         |      tick-size    = 0.002
         |    }
         |  ]
         |}
      """.stripMargin

    Seq(-1, -5, -10, -5000000000L).foreach { startOffset =>
      getSettingByConfig(configStr(matchingRulesSettings(startOffset))) should produce(
        s"waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.0.start-offset.+\n.+$startOffset should be >= 0".r
      )
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

    def matchingRulesSettings(tickSize: Double): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = 100
         |      tick-size    = $tickSize
         |    }
         |  ]
         |}
      """.stripMargin

    Seq(-0.1d, -0.00000001, -0.00000000000001, -12.123, -123.12312, -12312.14234)
      .foreach { tickSize =>
        getSettingByConfig(configStr(matchingRulesSettings(tickSize))) should produce(
          s"waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.0.tick-size.+\n.+$tickSize should be > 0".r
        )
      }

    // Because of formatting issues like 0 vs 0.0
    Seq(0, -1, -5, -10, -5000000000L).foreach { tickSize =>
      getSettingByConfig(configStr(matchingRulesSettings(tickSize.toDouble))) should produce(
        s"waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.0.tick-size.+\n.+$tickSize should be > 0".r
      )
    }
  }

  it can "be positive and fractional" in {

    def matchingRulesSettings(tickSize: Double): String =
      s"""matching-rules = {
         |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
         |    {
         |      start-offset = 100
         |      tick-size    = $tickSize
         |    }
         |  ]
         |}
      """.stripMargin

    Seq(
      1d, 5d, 10d, 5000000000d, 1000000000000000000000000000d, 0.1d, 0.00000001d, 0.00000000000001d, 12.123d, 123.1231213124234234d,
      123123123123123.14234234234234234234d
    ).foreach { tickSize =>
      getSettingByConfig(configStr(matchingRulesSettings(tickSize))).explicitGet().matchingRules shouldBe Map(
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

    getSettingByConfig(configStr(emptyTickSize)) should produce(
      "waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.+\n.+Empty".r
    )
  }
}
