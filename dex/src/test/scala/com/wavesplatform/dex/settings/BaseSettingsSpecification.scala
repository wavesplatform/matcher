package com.wavesplatform.dex.settings

import cats.syntax.either._
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.flatspec.AnyFlatSpec
import pureconfig.ConfigSource

import scala.util.Try

class BaseSettingsSpecification extends AnyFlatSpec {

  def getSettingByConfig(conf: Config): Either[String, MatcherSettings] =
    Try(ConfigSource.fromConfig(conf).at("waves.dex").loadOrThrow[MatcherSettings]).toEither.leftMap(_.getMessage)

  val correctOrderFeeStr: String =
    s"""
       |order-fee {
       |  -1: {
       |    mode = percent
       |    dynamic {
       |      base-maker-fee = 200000
       |      base-maker-fee = 700000
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

  val correctDeviationsStr: String =
    s"""
       |max-price-deviations {
       |  enable = yes
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

  val correctAllowedAssetPairsStr: String =
    s"""
       |allowed-asset-pairs = []
     """.stripMargin

  val correctOrderRestrictionsStr: String =
    s"""
       |order-restrictions = {}
     """.stripMargin

  val correctMatchingRulesStr: String =
    s"""
       |matching-rules = {}
     """.stripMargin

  val correctSubscriptionsSettingsStr: String =
    s"""
       |subscriptions {
       |  max-order-book-number = 20
       |  max-address-number = 20
       |}
       """.stripMargin

  def configWithSettings(
    orderFeeStr: String = correctOrderFeeStr,
    deviationsStr: String = correctDeviationsStr,
    allowedAssetPairsStr: String = correctAllowedAssetPairsStr,
    orderRestrictionsStr: String = correctOrderRestrictionsStr,
    matchingRulesStr: String = correctMatchingRulesStr,
    subscriptionsSettings: String = correctSubscriptionsSettingsStr
  ): Config = {
    val configStr =
      s"""waves {
         |  directory = /waves
         |  dex {
         |    id = "matcher-1"
         |    account-storage {
         |      type = "in-mem"
         |      in-mem.seed-in-base-64 = "c3lrYWJsZXlhdA=="
         |    }
         |    order-db {
         |      max-orders = 199
         |    }
         |    rest-api {
         |      address = 127.1.2.3
         |      port = 6880
         |      api-key-hash = foobarhash
         |      cors = no
         |      api-key-different-host = no
         |    }
         |    waves-blockchain-client {
         |      grpc {
         |        target = "127.1.2.9:6333"
         |        max-hedged-attempts = 9
         |        max-retry-attempts = 13
         |        keep-alive-without-calls = false
         |        keep-alive-time = 8s
         |        keep-alive-timeout = 11s
         |        idle-timeout = 20s
         |        channel-options {
         |          connect-timeout = 99s
         |        }
         |        no-data-timeout = 999m
         |      }
         |      blockchain-updates-grpc {
         |        target = "127.1.2.10:7444"
         |        max-hedged-attempts = 10
         |        max-retry-attempts = 14
         |        keep-alive-without-calls = true
         |        keep-alive-time = 9s
         |        keep-alive-timeout = 12s
         |        idle-timeout = 21s
         |        channel-options {
         |          connect-timeout = 100s
         |        }
         |        no-data-timeout = 782m
         |      }
         |      default-caches-expiration = 101ms
         |      balance-stream-buffer-size = 100
         |      combined-client-settings {
         |        max-rollback-height = 90
         |        max-cached-latest-block-updates = 7
         |        combined-stream.restart-delay = 199ms
         |        pessimistic-portfolios.max-confirmed-transactions = 400
         |      }
         |    }
         |    exchange-tx-base-fee = 300000
         |    actor-response-timeout = 11s
         |    snapshots-interval = 999
         |    limit-events-during-recovery = 48879
         |    make-snapshots-at-start = yes
         |    snapshots-loading-timeout = 423s
         |    start-events-processing-timeout = 543s
         |    order-books-recovering-timeout = 111s
         |    rest-order-limit = 100
         |    price-assets = [
         |      WAVES
         |      8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS
         |      DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
         |    ]
         |    blacklisted-assets = ["AbunLGErT5ctzVN8MVjb4Ad9YgjpubB8Hqb17VxzfAck"]
         |    blacklisted-names = ["b"]
         |    blacklisted-addresses = [
         |      3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD
         |    ]
         |    white-list-only = yes
         |    allowed-order-versions = [11, 22]
         |    order-book-http {
         |      depth-ranges = [1, 5, 333]
         |      default-depth = 5
         |    }
         |    events-queue {
         |      type = "kafka"
         |
         |      local {
         |        enable-storing = no
         |        polling-interval = 1d
         |        max-elements-per-poll = 99
         |        clean-before-consume = no
         |      }
         |
         |      kafka {
         |        topic = "some-events"
         |
         |        consumer {
         |          fetch-max-duration = 10s
         |          max-buffer-size = 777
         |          client.foo = 2
         |        }
         |
         |        producer {
         |          enable = no
         |          client.bar = 3
         |        }
         |      }
         |
         |      circuit-breaker {
         |        max-failures = 999
         |        call-timeout = 123s
         |        reset-timeout = 1d
         |      }
         |    }
         |    process-consumed-timeout = 663s
         |    $orderFeeStr
         |    $deviationsStr
         |    $allowedAssetPairsStr
         |    $orderRestrictionsStr
         |    $matchingRulesStr
         |    exchange-transaction-broadcast {
         |      interval = 1 day
         |      max-pending-time = 30 days
         |    }
         |    web-sockets {
         |      external-client-handler {
         |        messages-interval = 1d
         |        max-connection-lifetime = 3d
         |        jwt-public-key = \"\"\"foo
         |bar
         |baz\"\"\"
         |        $subscriptionsSettings
         |        health-check {
         |          ping-interval = 9m
         |          pong-timeout = 129m
         |        }
         |      }
         |      internal-broadcast {
         |        messages-interval = 923ms
         |      }
         |      internal-client-handler {
         |        health-check {
         |          ping-interval = 10m
         |          pong-timeout = 374m
         |        }
         |      }
         |    }
         |    address-actor {
         |      max-active-orders = 400
         |      ws-messages-interval = 100ms
         |      batch-cancel-timeout = 18 seconds
         |    }
         |
         |    comparison-tool {
         |      checks {
         |        interval = 55m
         |        duration = 3d
         |        strike = 9
         |      }
         |
         |      matcher-rest-apis = ["https://127.0.0.1:1234"]
         |
         |      tradable-balance-check {
         |        account-pks = ["DuzcrAJcA8B7dEdaGfutD8NKQHB1Vix9JUoNWiMK9PMH"]
         |        asset-pairs = ["WAVES-8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"]
         |      }
         |    }
         |  }
         |}""".stripMargin

    loadConfig(ConfigFactory.parseString(configStr))
  }

}
