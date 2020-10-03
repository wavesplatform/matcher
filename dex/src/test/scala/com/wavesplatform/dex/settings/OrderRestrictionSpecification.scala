package com.wavesplatform.dex.settings

import com.typesafe.config.Config
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import org.scalatest.matchers.should.Matchers

class OrderRestrictionSpecification extends BaseSettingsSpecification with Matchers {

  def configStr(x: String): Config = configWithSettings(orderRestrictionsStr = x)

  "Empty pairs" should "not produce restrictions" in {
    getSettingByConfig(configStr("")).explicitGet().orderRestrictions shouldBe Map.empty
  }

  "Empty pair settings" should "be validated by default" in {
    val emptyPairSettings =
      """order-restrictions = {
        | "WAVES-BTC": {
        | }
        |}
      """.stripMargin
    getSettingByConfig(configStr(emptyPairSettings)).explicitGet().orderRestrictions shouldBe
      Map(
        AssetPair.createAssetPair("WAVES", "BTC").get ->
          OrderRestrictionsSettings(
            stepAmount = OrderRestrictionsSettings.Default.stepAmount,
            minAmount = OrderRestrictionsSettings.Default.minAmount,
            maxAmount = OrderRestrictionsSettings.Default.maxAmount,
            stepPrice = OrderRestrictionsSettings.Default.stepPrice,
            minPrice = OrderRestrictionsSettings.Default.minPrice,
            maxPrice = OrderRestrictionsSettings.Default.maxPrice
          )
      )
  }

  "Incorrect values of settings" should "produce error" in {
    withClue("negative and String values") {
      def testTemplate(setting: String, value: String): String =
        s"""order-restrictions = {
           | "WAVES-BTC": {
           |   $setting = $value
           | }
           |}
      """.stripMargin
      Array("min-amount", "max-amount", "step-amount", "min-price", "max-price", "step-price").foreach { s =>
        Array("0", "-5", "-100", "-1", "-0.11", "-512.123", "-100000000", "-1000000000000000").foreach { v =>
          getSettingByConfig(configStr(testTemplate(s, v))) should produce(s"waves.dex.order-restrictions.WAVES-BTC.$s.+\n.+> 0.0".r)
        }
        Array(-0.0001, -0.000000001, -0.0000000000000000000001, -15.345, -1234.1234152416346134).foreach { v =>
          getSettingByConfig(configStr(testTemplate(s, v.toString))) should produce(s"waves.dex.order-restrictions.WAVES-BTC.$s.+\n.+> 0.0".r)
        }
      }
    }

    val testMinMaxArray = Array(
      "6"            -> "3",
      "2"            -> "1",
      "1.00001"      -> "1",
      "1.0000001"    -> "1",
      "1.000000001"  -> "1",
      "1"            -> "0.9",
      "1"            -> "0.99999",
      "1"            -> "0.99999999999",
      "1000000000"   -> "999999999",
      "100000000000" -> "1"
    )
    withClue("min-amount > max-amount") {
      def testTemplate(minAmount: String, maxAmount: String): String =
        s"""order-restrictions = {
           | "WAVES-BTC": {
           |   min-amount = $minAmount
           |   max-amount = $maxAmount
           | }
           |}
      """.stripMargin
      testMinMaxArray.foreach { case (min, max) =>
        getSettingByConfig(configStr(testTemplate(min, max))) should produce(
          s"waves.dex.order-restrictions.WAVES-BTC.max-amount.+\n.+$max should be > min-amount: $min".r
        )
      }
    }
    withClue("min-price > max-price") {
      def testTemplate(minPrice: String, maxPrice: String): String =
        s"""order-restrictions = {
           | "WAVES-BTC": {
           |   min-price = $minPrice
           |   max-price = $maxPrice
           | }
           |}
      """.stripMargin
      testMinMaxArray.foreach { case (min, max) =>
        getSettingByConfig(configStr(testTemplate(min, max))) should produce(
          s"waves.dex.order-restrictions.WAVES-BTC.max-price.+\n.+$max should be > min-price: $min".r
        )
      }
    }

    withClue("invalid pair") {
      def testTemplate(pair: String): String =
        s"""order-restrictions = {
           | "$pair": {}
           |}
      """.stripMargin
      Seq("ETH-;;;", "ETH", "@#%").foreach { p =>
        getSettingByConfig(configStr(testTemplate(p))) should produce(s"waves.dex.order-restrictions.+\n.+$p".r)
      }
    }
  }

  "Incorrect values" should "produce errors" in {
    val incorrectPairAndStepAmount =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = -0.013
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        | },
        | "ETH-;;;": {
        |   step-amount = 0.01
        |   min-amount  = 0.05
        |   max-amount  = 20000
        | }
        |}
      """.stripMargin

    withClue("incorrect pair and step amount") {
      getSettingByConfig(configStr(incorrectPairAndStepAmount)) should produce("waves.dex.order-restrictions.+\n.+ETH-;;;".r)
      getSettingByConfig(configStr(incorrectPairAndStepAmount)) should produce(
        "waves.dex.order-restrictions.WAVES-BTC.step-amount.+\n.+-0.013 should be > 0.0".r
      )
    }

    val someIncorrectValues =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = -0.013
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        | },
        | "ETH-;;;": {
        |   step-amount = 0.01
        |   min-amount  = -0.05
        |   max-amount  = 20000
        | }
        |}
      """.stripMargin
    withClue("incorrect step amount, pair and min-amount in incorrect pair") {
      getSettingByConfig(configStr(someIncorrectValues)) should produce("waves.dex.order-restrictions.+.\n.+ETH-;;;".r)
      getSettingByConfig(configStr(someIncorrectValues)) should produce(
        "waves.dex.order-restrictions.\"ETH-;;;\".min-amount.+.\n.+-0.05 should be > 0.0".r
      )
      getSettingByConfig(configStr(someIncorrectValues)) should produce(
        "waves.dex.order-restrictions.WAVES-BTC.step-amount.+.\n.+-0.013 should be > 0.0".r
      )
    }
  }

  "Order restrictions" should "be validated" in {

    val nonEmptyCorrect =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = 0.001
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        | },
        | "ETH-USD": {
        |   step-amount = 0.01
        |   min-amount  = 0.05
        |   max-amount  = 20000
        |   step-price  = 0.002
        | }
        |}
      """.stripMargin
    withClue("nonempty correct") {
      getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().orderRestrictions shouldBe
        Map(
          AssetPair.createAssetPair("WAVES", "BTC").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.001,
              minAmount = 0.001,
              maxAmount = 1000000,
              stepPrice = OrderRestrictionsSettings.Default.stepPrice,
              minPrice = OrderRestrictionsSettings.Default.minPrice,
              maxPrice = OrderRestrictionsSettings.Default.maxPrice
            ),
          AssetPair.createAssetPair("ETH", "USD").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.01,
              minAmount = 0.05,
              maxAmount = 20000,
              stepPrice = 0.002,
              minPrice = OrderRestrictionsSettings.Default.minPrice,
              maxPrice = OrderRestrictionsSettings.Default.maxPrice
            )
        )
    }

    val oneFullPairSettings =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = 0.001
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        |   step-price = 0.002
        |   min-price  = 0.003
        |   max-price  = 500000
        | }
        |}
      """.stripMargin
    withClue("one full pair settings") {
      getSettingByConfig(configStr(oneFullPairSettings)).explicitGet().orderRestrictions shouldBe
        Map(
          AssetPair.createAssetPair("WAVES", "BTC").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.001,
              minAmount = 0.001,
              maxAmount = 1000000,
              stepPrice = 0.002,
              minPrice = 0.003,
              maxPrice = 500000
            )
        )
    }

    val someFullPairSettings =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = 0.001
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        |   step-price = 0.002
        |   min-price  = 0.003
        |   max-price  = 500000
        | },
        | "ETH-WAVES": {
        |   step-amount = 0.0001
        |   min-amount  = 0.01
        |   max-amount  = 2000
        |   step-price = 0.003
        |   min-price  = 0.004
        |   max-price  = 10000
        | },
        | "ETH-USD": {
        |   step-amount = 0.1
        |   min-amount  = 0.1
        |   max-amount  = 200
        |   step-price = 0.003
        |   min-price  = 0.004
        |   max-price  = 15000
        | }
        |}
      """.stripMargin
    withClue("some full pair settings") {
      getSettingByConfig(configStr(someFullPairSettings)).explicitGet().orderRestrictions shouldBe
        Map(
          AssetPair.createAssetPair("WAVES", "BTC").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.001,
              minAmount = 0.001,
              maxAmount = 1000000,
              stepPrice = 0.002,
              minPrice = 0.003,
              maxPrice = 500000
            ),
          AssetPair.createAssetPair("ETH", "WAVES").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.0001,
              minAmount = 0.01,
              maxAmount = 2000,
              stepPrice = 0.003,
              minPrice = 0.004,
              maxPrice = 10000
            ),
          AssetPair.createAssetPair("ETH", "USD").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.1,
              minAmount = 0.1,
              maxAmount = 200,
              stepPrice = 0.003,
              minPrice = 0.004,
              maxPrice = 15000
            )
        )
    }

    val setAndDefaultMix =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        |   step-price = 0.002
        |   min-price  = 0.003
        | },
        | "ETH-WAVES": {
        |   step-amount = 0.0001
        |   max-amount  = 2000
        |   max-price  = 10000
        | }
        |}
      """.stripMargin
    withClue("some full pair settings") {
      getSettingByConfig(configStr(setAndDefaultMix)).explicitGet().orderRestrictions shouldBe
        Map(
          AssetPair.createAssetPair("WAVES", "BTC").get ->
            OrderRestrictionsSettings(
              stepAmount = OrderRestrictionsSettings.Default.stepAmount,
              minAmount = 0.001,
              maxAmount = 1000000,
              stepPrice = 0.002,
              minPrice = 0.003,
              maxPrice = OrderRestrictionsSettings.Default.maxPrice
            ),
          AssetPair.createAssetPair("ETH", "WAVES").get ->
            OrderRestrictionsSettings(
              stepAmount = 0.0001,
              minAmount = OrderRestrictionsSettings.Default.minAmount,
              maxAmount = 2000,
              stepPrice = OrderRestrictionsSettings.Default.stepPrice,
              minPrice = OrderRestrictionsSettings.Default.minPrice,
              maxPrice = 10000
            )
        )
    }
  }
}
