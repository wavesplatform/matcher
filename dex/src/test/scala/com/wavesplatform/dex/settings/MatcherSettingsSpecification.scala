package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import com.typesafe.config.Config
import com.wavesplatform.dex.actors.address.AddressActor
import com.wavesplatform.dex.api.http.OrderBookHttpInfo
import com.wavesplatform.dex.api.ws.actors.{WsExternalClientHandlerActor, WsHealthCheckSettings, WsInternalBroadcastActor, WsInternalClientHandlerActor}
import com.wavesplatform.dex.db.{AccountStorage, OrderDB}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.combined.{CombinedStream, CombinedWavesBlockchainClient}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.SynchronizedPessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.settings.{GrpcClientSettings, WavesBlockchainClientSettings}
import com.wavesplatform.dex.model.Implicits.AssetPairOps
import com.wavesplatform.dex.queue.LocalMatcherQueue
import com.wavesplatform.dex.settings.EventsQueueSettings.CircuitBreakerSettings
import com.wavesplatform.dex.settings.OrderFeeSettings.PercentSettings
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import org.scalatest.matchers.should.Matchers
import pureconfig.ConfigSource

import scala.concurrent.duration._

class MatcherSettingsSpecification extends BaseSettingsSpecification with Matchers with DiffMatcherWithImplicits {

  "MatcherSettings" should "read values" in {

    val config = configWithSettings()
    val settings = ConfigSource.fromConfig(config).at("waves.dex").loadOrThrow[MatcherSettings]

    settings.id should be("matcher-1")
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
        blockchainUpdatesGrpc = GrpcClientSettings(
          target = "127.1.2.10:7444",
          maxHedgedAttempts = 10,
          maxRetryAttempts = 14,
          keepAliveWithoutCalls = true,
          keepAliveTime = 9.seconds,
          keepAliveTimeout = 12.seconds,
          idleTimeout = 21.seconds,
          channelOptions = GrpcClientSettings.ChannelOptionsSettings(
            connectTimeout = 100.seconds
          )
        ),
        defaultCachesExpiration = 101.millis,
        balanceStreamBufferSize = 100,
        combinedClientSettings = CombinedWavesBlockchainClient.Settings(
          maxRollbackHeight = 90,
          combinedStream = CombinedStream.Settings(199.millis),
          pessimisticPortfolios = SynchronizedPessimisticPortfolios.Settings(400)
        )
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
    settings.orderBookHttp shouldBe OrderBookHttpInfo.Settings(
      depthRanges = List(1, 5, 333),
      defaultDepth = Some(5)
    )
    settings.eventsQueue.`type` shouldBe "kafka"
    settings.eventsQueue.local shouldBe LocalMatcherQueue.Settings(enableStoring = false, 1.day, 99, cleanBeforeConsume = false)
    settings.eventsQueue.kafka.topic shouldBe "some-events"
    settings.eventsQueue.kafka.consumer.fetchMaxDuration shouldBe 10.seconds
    settings.eventsQueue.kafka.consumer.maxBufferSize shouldBe 777
    settings.eventsQueue.kafka.consumer.client.getInt("foo") shouldBe 2
    settings.eventsQueue.kafka.producer.client.getInt("bar") shouldBe 3
    settings.eventsQueue.circuitBreaker should matchTo(
      CircuitBreakerSettings(
        maxFailures = 999,
        callTimeout = 123.seconds,
        resetTimeout = 1.day
      )
    )
    settings.processConsumedTimeout shouldBe 663.seconds
    settings.orderFee should matchTo(Map[Long, OrderFeeSettings](-1L -> PercentSettings(AssetType.Amount, 0.1)))
    settings.maxPriceDeviations shouldBe DeviationsSettings(enable = true, 1000000, 1000000, 1000000)
    settings.allowedAssetPairs shouldBe Set.empty[AssetPair]
    settings.allowedOrderVersions shouldBe Set(11, 22)
    settings.orderRestrictions shouldBe Map.empty[AssetPair, OrderRestrictionsSettings]
    settings.exchangeTransactionBroadcast shouldBe ExchangeTransactionBroadcastSettings(
      broadcastUntilConfirmed = true,
      interval = 1.day,
      maxPendingTime = 30.days
    )
    val expectedJwtPublicKey = """foo
bar
baz"""
    settings.webSockets should matchTo(
      WebSocketSettings(
        externalClientHandler = WsExternalClientHandlerActor
          .Settings(1.day, 3.days, expectedJwtPublicKey, SubscriptionsSettings(20, 20), WsHealthCheckSettings(9.minutes, 129.minutes)),
        internalBroadcast = WsInternalBroadcastActor.Settings(923.millis),
        internalClientHandler = WsInternalClientHandlerActor.Settings(WsHealthCheckSettings(10.minutes, 374.minutes))
      )
    )
    settings.addressActor should matchTo(AddressActor.Settings(100.milliseconds, 18.seconds, 400))
  }

  "DeviationsSettings in MatcherSettings" should "be validated" in {

    val invalidEnable: String =
      s"""
         |max-price-deviations {
         |  enable = foobar
         |  max-price-profit = 1000000
         |  max-price-loss = 1000000
         |  max-fee-deviation = 1000000
         |
         |  # TODO COMPAT
         |  profit = 1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidProfit: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  max-price-profit = -1000000
         |  max-price-loss = 1000000
         |  max-fee-deviation = 1000000
         |
         |  # TODO COMPAT
         |  profit = -1000000
         |  loss = 1000000
         |  fee = 1000000
         |}
     """.stripMargin

    val invalidLossAndFee: String =
      s"""
         |max-price-deviations {
         |  enable = yes
         |  max-price-profit = 1000000
         |  max-price-loss = 0
         |  max-fee-deviation = -1000000
         |
         |  # TODO COMPAT
         |  profit = 1000000
         |  loss = 0
         |  fee = -1000000
         |}
     """.stripMargin

    def configStr(x: String): Config = configWithSettings(deviationsStr = x)
    val settingsInvalidEnable = getSettingByConfig(configStr(invalidEnable))
    val settingsInvalidProfit = getSettingByConfig(configStr(invalidProfit))
    val settingsInvalidLossAndFee = getSettingByConfig(configStr(invalidLossAndFee))

    settingsInvalidEnable should produce("waves.dex.max-price-deviations.enable")
    settingsInvalidProfit should produce("waves.dex.max-price-deviations.max-price-profit")
    Seq("waves.dex.max-price-deviations.max-fee-deviation", "waves.dex.max-price-deviations.max-price-loss").foreach { message =>
      settingsInvalidLossAndFee should produce(message)
    }
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

    val invalidAssetType =
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

    val invalidPercent =
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
         |      asset-type = spending
         |      min-fee = 121.2
         |    }
         |  }
         |}
       """.stripMargin

    val invalidAsset =
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
         |      min-fee = 50
         |    }
         |  }
         |}
       """.stripMargin

    val invalidFee =
      s"""
         |order-fee {
         |  -1: {
         |    mode = fixed
         |    dynamic {
         |      base-maker-fee = 300000
         |      base-taker-fee = 300000
         |    }
         |    fixed {
         |      asset = DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
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

    def configStr(x: String): Config = configWithSettings(orderFeeStr = x)

    getSettingByConfig(configStr(invalidMode())) should produce("waves.dex.order-fee.-1.mode.+\n.+invalid".r)
    getSettingByConfig(configStr(invalidMode("waves"))) should produce("waves.dex.order-fee.-1.mode.+\n.+waves".r)
    getSettingByConfig(configStr(invalidAssetType)) should produce("waves.dex.order-fee.-1.percent.asset-type.+\n.+test".r)
    getSettingByConfig(configStr(invalidPercent)) should produce("order-fee.-1.percent.min-fee")
    getSettingByConfig(configStr(invalidAsset)) should produce("order-fee.-1.fixed.asset.+\n.+;".r)
    getSettingByConfig(configStr(invalidFee)) should produce("order-fee.-1.fixed.min-fee")
    getSettingByConfig(configStr(invalidFeeInDynamicMode)) should produce("order-fee.-1.dynamic.base-maker-fee")
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
      """waves.dex.allowed-asset-pairs.1.+
.+WAVES-BTC-ETH.+incorrect assets count, expected 2 but got 3.+
.+waves.dex.allowed-asset-pairs.2.+
.+ETH.+incorrect assets count, expected 2 but got 1""".r
    )

    getSettingByConfig(configStr(incorrectAssets)) should produce(
      "waves.dex.allowed-asset-pairs.0.+\n.+WAVES-;;;.+requirement failed: Wrong char ';' in Base58 string ';;;'".r
    )

    getSettingByConfig(configStr(duplicates)).explicitGet().allowedAssetPairs.size shouldBe 2

    getSettingByConfig(configStr(nonEmptyCorrect)).explicitGet().allowedAssetPairs shouldBe
    Set(
      AssetPair.createAssetPair("WAVES", "BTC").get,
      AssetPair.createAssetPair("WAVES", "ETH").get,
      AssetPair.createAssetPair("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "WAVES").get
    )
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
        "waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.+\n.+but they are: 100, 100".r
      )
    }

    withClue("incorrect rules order: 100, 88") {
      getSettingByConfig(configStr(incorrectRulesOrder(100, 88))) should produce(
        "waves.dex.matching-rules.WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS.+\n.+but they are: 100, 88".r
      )
    }
  }

  "Subscriptions settings" should "be validated" in {

    val invalidSubscriptionsSettings =
      s"""
         | subscriptions {
         |   max-order-book-number = 0
         |   max-address-number = 0
         | }
         """.stripMargin

    getSettingByConfig(configWithSettings(subscriptionsSettings = invalidSubscriptionsSettings)) should produce(
      """waves.dex.web-sockets.external-client-handler.subscriptions.max-address-number.+
.+should be > 0
.+waves.dex.web-sockets.external-client-handler.subscriptions.max-order-book-number.+
.+should be > 0""".r
    )
  }
}
