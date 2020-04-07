package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.typesafe.config.Config
import com.wavesplatform.dex.AddressActor
import com.wavesplatform.dex.api.OrderBookSnapshotHttpCache
import com.wavesplatform.dex.api.websockets.actors.PingPongHandlerActor
import com.wavesplatform.dex.db.{AccountStorage, OrderDB}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import com.wavesplatform.dex.model.Implicits.AssetPairOps
import com.wavesplatform.dex.queue.LocalMatcherQueue
import com.wavesplatform.dex.settings.OrderFeeSettings.{OrderFeeSettings, PercentSettings}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import net.ceedubs.ficus.Ficus._
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class MatcherSettingsSpecification extends BaseSettingsSpecification with Matchers with DiffMatcherWithImplicits {

  "MatcherSettings" should "read values" in {

    val config   = configWithSettings()
    val settings = config.as[MatcherSettings]("waves.dex")

    settings.accountStorage should be(AccountStorage.Settings.InMem(ByteStr.decodeBase64("c3lrYWJsZXlhdA==").get))
    settings.restApi shouldBe RestAPISettings(
      address = "127.1.2.3",
      port = 6880,
      apiKeyHash = "foobarhash",
      cors = false,
      apiKeyDifferentHost = false
    )

    settings.wavesBlockchainClient should matchTo(
      WavesBlockchainClientSettings(
        grpc = GrpcClientSettings(
          target = "127.1.2.9:6333",
          maxHedgedAttempts = 9,
          maxRetryAttempts = 13,
          keepAliveWithoutCalls = false,
          keepAliveTime = 8.seconds,
          keepAliveTimeout = 11.seconds,
          idleTimeout = 20.seconds,
          channelOptions = GrpcClientSettings.ChannelOptionsSettings(
            connectTimeout = 99.seconds
          )
        ),
        defaultCachesExpiration = 101.millis,
        balanceStreamBufferSize = 100
      )
    )

    settings.exchangeTxBaseFee should be(300000)
    settings.actorResponseTimeout should be(11.seconds)
    settings.snapshotsInterval should be(999)
    settings.snapshotsLoadingTimeout should be(423.seconds)
    settings.startEventsProcessingTimeout should be(543.seconds)
    settings.orderDb should be(OrderDB.Settings(199))
    settings.priceAssets should be(
      Seq(
        Waves,
        AssetPair.extractAsset("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get,
        AssetPair.extractAsset("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J").get
      )
    )
    settings.blacklistedAssets shouldBe Set(AssetPair.extractAsset("AbunLGErT5ctzVN8MVjb4Ad9YgjpubB8Hqb17VxzfAck").get.asInstanceOf[IssuedAsset])
    settings.blacklistedNames.map(_.pattern.pattern()) shouldBe Seq("b")
    settings.blacklistedAddresses shouldBe Set("3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD")
    settings.orderBookSnapshotHttpCache shouldBe OrderBookSnapshotHttpCache.Settings(
      cacheTimeout = 11.minutes,
      depthRanges = List(1, 5, 333),
      defaultDepth = Some(5)
    )
    settings.eventsQueue.tpe shouldBe "kafka"
    settings.eventsQueue.local shouldBe LocalMatcherQueue.Settings(enableStoring = false, 1.day, 99, cleanBeforeConsume = false)
    settings.eventsQueue.kafka.topic shouldBe "some-events"
    settings.eventsQueue.kafka.consumer.fetchMaxDuration shouldBe 10.seconds
    settings.eventsQueue.kafka.consumer.maxBufferSize shouldBe 777
    settings.eventsQueue.kafka.consumer.client.getInt("foo") shouldBe 2
    settings.eventsQueue.kafka.producer.client.getInt("bar") shouldBe 3
    settings.processConsumedTimeout shouldBe 663.seconds
    settings.orderFee should matchTo(Map[Long, OrderFeeSettings](-1L -> PercentSettings(AssetType.AMOUNT, 0.1)))
    settings.deviation shouldBe DeviationsSettings(true, 1000000, 1000000, 1000000)
    settings.allowedAssetPairs shouldBe Set.empty[AssetPair]
    settings.allowedOrderVersions shouldBe Set(11, 22)
    settings.orderRestrictions shouldBe Map.empty[AssetPair, OrderRestrictionsSettings]
    settings.exchangeTransactionBroadcast shouldBe ExchangeTransactionBroadcastSettings(
      broadcastUntilConfirmed = true,
      interval = 1.day,
      maxPendingTime = 30.days
    )
    settings.webSocketSettings should matchTo(WebSocketSettings(100.milliseconds, 24.hours, PingPongHandlerActor.Settings(10.seconds, 30.seconds)))
    settings.addressActorSettings should matchTo(AddressActor.Settings(100.milliseconds, 18.seconds, 400))
  }

  "DeviationsSettings in MatcherSettings" should "be validated" in {

    val invalidEnable: String =
      s"""
         |max-price-deviations {
         |  enable = foobar
         |  profit = 1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidProfit: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  profit = -1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidLossAndFee: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  profit = 1000000
         |  loss = 0
         |  fee = -1000000
         |}
     """.stripMargin

    def configStr(x: String): Config = configWithSettings(deviationsStr = x)
    val settingsInvalidEnable        = getSettingByConfig(configStr(invalidEnable))
    val settingsInvalidProfit        = getSettingByConfig(configStr(invalidProfit))
    val settingsInvalidLossAndFee    = getSettingByConfig(configStr(invalidLossAndFee))

    settingsInvalidEnable shouldBe Left("Invalid setting max-price-deviations.enable value: foobar")

    settingsInvalidProfit shouldBe
      Left("Invalid setting max-price-deviations.profit value: -1000000 (required 0 < percent)")

    settingsInvalidLossAndFee shouldBe
      Left(
        "Invalid setting max-price-deviations.loss value: 0 (required 0 < percent), " +
          "Invalid setting max-price-deviations.fee value: -1000000 (required 0 < percent)")
  }

  "OrderFeeSettings in MatcherSettings" should "be validated" in {

    def invalidMode(invalidModeName: String = "invalid"): String =
      s"""
         |order-fee {
         |  -1: {
         |    mode = $invalidModeName
         |    dynamic {
         |      base-maker-fee = 300000
         |      base-taker-fee = 300000
         |    }
         |    fixed {
         |      asset = WAVES
         |      min-fee = 300000
         |    }
         |    percent {
         |      asset-type = amount
         |      min-fee = 0.1
         |    }
         |  }
         |}
       """.stripMargin

    val invalidAssetTypeAndPercent =
      s"""
         |order-fee {
         |  -1: {
         |    mode = percent
         |    dynamic {
         |      base-maker-fee = 300000
         |      base-taker-fee = 300000
         |    }
         |    fixed {
         |      asset = WAVES
         |      min-fee = 300000
         |    }
         |    percent {
         |      asset-type = test
         |      min-fee = 121.2
         |    }
         |  }
         |}
       """.stripMargin

    val invalidAssetAndFee =
      s"""
         |order-fee {
         |  -1: {
         |    mode = fixed
         |    dynamic {
         |      base-maker-fee = 300000
         |      base-taker-fee = 300000
         |    }
         |    fixed {
         |      asset = ;;;;
         |      min-fee = -300000
         |    }
         |    percent {
         |      asset-type = test
         |      min-fee = 121
         |    }
         |  }
         |}
       """.stripMargin

    val invalidFeeInDynamicMode =
      s"""
         |order-fee {
         |  -1: {
         |    mode = dynamic
         |    dynamic {
         |      base-maker-fee = -350000
         |      base-taker-fee = 350000
         |    }
         |    fixed {
         |      asset = ;;;;
         |      min-fee = -300000
         |    }
         |    percent {
         |      asset-type = test
         |      min-fee = 121
         |    }
         |  }
         |}
       """.stripMargin

    def configStr(x: String): Config    = configWithSettings(orderFeeStr = x)
    val settingsInvalidMode             = getSettingByConfig(configStr(invalidMode()))
    val settingsDeprecatedNameMode      = getSettingByConfig(configStr(invalidMode("waves")))
    val settingsInvalidTypeAndPercent   = getSettingByConfig(configStr(invalidAssetTypeAndPercent))
    val settingsInvalidAssetAndFee      = getSettingByConfig(configStr(invalidAssetAndFee))
    val settingsInvalidFeeInDynamicMode = getSettingByConfig(configStr(invalidFeeInDynamicMode))

    settingsInvalidMode shouldBe Left("Invalid setting order-fee value: Invalid setting order-fee.-1.mode value: invalid")

    settingsDeprecatedNameMode shouldBe Left("Invalid setting order-fee value: Invalid setting order-fee.-1.mode value: waves")

    settingsInvalidTypeAndPercent shouldBe
      Left(
        "Invalid setting order-fee value: Invalid setting order-fee.-1.percent.asset-type value: test, " +
          "Invalid setting order-fee.-1.percent.min-fee value: 121.2 (required 0 < percent <= 100)")

    settingsInvalidAssetAndFee shouldBe
      Left(
        "Invalid setting order-fee value: Invalid setting order-fee.-1.fixed.asset value: ;;;;, " +
          "Invalid setting order-fee.-1.fixed.min-fee value: -300000 (required 0 < fee)")

    settingsInvalidFeeInDynamicMode shouldBe Left(
      s"Invalid setting order-fee value: Invalid setting order-fee.-1.dynamic.base-maker-fee value: -350000 (required 0 < base maker fee)"
    )
  }

  "Allowed asset pairs in MatcherSettings" should "be validated" in {

    def configStr(x: String): Config = configWithSettings(allowedAssetPairsStr = x)

    val incorrectAssetsCount =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-BTC-ETH",
        | "ETH"
        |]
      """.stripMargin

    val incorrectAssets =
      """allowed-asset-pairs = [
        | "WAVES-;;;",
        | "WAVES-BTC"
        |]
      """.stripMargin

    val duplicates =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-ETH",
        | "WAVES-BTC"
        |]
      """.stripMargin

    val nonEmptyCorrect =
      """allowed-asset-pairs = [
        | "WAVES-BTC",
        | "WAVES-ETH",
        | "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS-WAVES"
        |]
      """.stripMargin

    getSettingByConfig(configStr(incorrectAssetsCount)) should produce(
      "Invalid setting allowed-asset-pairs value: WAVES-BTC-ETH (incorrect assets count, expected 2 but got 3), ETH (incorrect assets count, expected 2 but got 1)"
    )

    getSettingByConfig(configStr(incorrectAssets)) should produce(
      "Invalid setting allowed-asset-pairs value: WAVES-;;; (requirement failed: Wrong char ';' in Base58 string ';;;')"
    )

    getSettingByConfig(configStr(duplicates)).explicitGet().allowedAssetPairs.size shouldBe 2

    getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().allowedAssetPairs shouldBe
      Set(
        AssetPair.createAssetPair("WAVES", "BTC").get,
        AssetPair.createAssetPair("WAVES", "ETH").get,
        AssetPair.createAssetPair("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "WAVES").get
      )
  }

  "Order restrictions" should "be validated" in {

    def configStr(x: String): Config = configWithSettings(orderRestrictionsStr = x)

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
        | }
        |}
      """.stripMargin

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

    val incorrectMinAndMax =
      """order-restrictions = {
        | "WAVES-BTC": {
        |   step-amount = 0.013
        |   min-amount  = 0.001
        |   max-amount  = 1000000
        |   min-price   = 100
        |   max-price   = 10
        | },
        | "ETH-WAVES": {
        |   step-price = 0.14
        |   max-price  = 17
        | }
        |}
      """.stripMargin

    withClue("default") {
      getSettingByConfig(configStr("")).explicitGet().orderRestrictions shouldBe Map.empty
    }

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
              stepPrice = OrderRestrictionsSettings.Default.stepPrice,
              minPrice = OrderRestrictionsSettings.Default.minPrice,
              maxPrice = OrderRestrictionsSettings.Default.maxPrice
            )
        )
    }

    withClue("incorrect pair and step amount") {
      getSettingByConfig(configStr(incorrectPairAndStepAmount)) should produce(
        "Invalid setting order-restrictions value: Can't parse asset pair 'ETH-;;;', " +
          "Invalid setting order-restrictions.WAVES-BTC.step-amount value: -0.013 (required 0 < value)"
      )
    }

    withClue("incorrect min and max") {
      getSettingByConfig(configStr(incorrectMinAndMax)) should produce(
        "Required order-restrictions.WAVES-BTC.min-price < order-restrictions.WAVES-BTC.max-price")
    }
  }

  "Matching rules" should "be validated" in {
    def configStr(x: String): Config = configWithSettings(matchingRulesStr = x)

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

    def incorrectRulesOrder(firstRuleOffset: Long, secondRuleOffset: Long): String =
      s"""matching-rules = {
        |  "WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS": [
        |    {
        |      start-offset = $firstRuleOffset
        |      tick-size    = 0.002
        |    },
        |    {
        |      start-offset = $secondRuleOffset
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

    withClue("incorrect rules order: 100, 100") {
      getSettingByConfig(configStr(incorrectRulesOrder(100, 100))) should produce(
        "Invalid setting matching-rules value: Rules should be ordered by offset, but they are: 100, 100")
    }

    withClue("incorrect rules order: 100, 88") {
      getSettingByConfig(configStr(incorrectRulesOrder(100, 88))) should produce(
        "Invalid setting matching-rules value: Rules should be ordered by offset, but they are: 100, 88")
    }
  }
}
