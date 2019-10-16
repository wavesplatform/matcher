package com.wavesplatform.dex.settings

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.settings.loadConfig
import org.scalatest.FlatSpec
import net.ceedubs.ficus.Ficus._

import scala.util.Try

class BaseSettingsSpecification extends FlatSpec{

  def getSettingByConfig(conf: Config): Either[String, MatcherSettings] =
    Try(conf.as[MatcherSettings]("waves.dex")).toEither.leftMap(_.getMessage)

  val correctOrderFeeStr: String =
    s"""
       |order-fee {
       |  mode = percent
       |  dynamic {
       |    base-fee = 300000
       |  }
       |  fixed {
       |    asset = WAVES
       |    min-fee = 300000
       |  }
       |  percent {
       |    asset-type = amount
       |    min-fee = 0.1
       |  }
       |}
       """.stripMargin

  val correctDeviationsStr: String =
    s"""
       |max-price-deviations {
       |  enable = yes
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

  def configWithSettings(orderFeeStr: String = correctOrderFeeStr,
                         deviationsStr: String = correctDeviationsStr,
                         allowedAssetPairsStr: String = correctAllowedAssetPairsStr,
                         orderRestrictionsStr: String = correctOrderRestrictionsStr,
                         matchingRulesStr: String = correctMatchingRulesStr): Config = {
    val configStr =
      s"""waves {
         |  directory = /waves
         |  dex {
         |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
         |    bind-address = 127.0.0.1
         |    port = 6886
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
         |    blacklisted-assets = ["a"]
         |    blacklisted-names = ["b"]
         |    blacklisted-addresses = [
         |      3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD
         |    ]
         |    white-list-only = yes
         |    allowed-order-versions = [11, 22]
         |    order-book-snapshot-http-cache {
         |      cache-timeout = 11m
         |      depth-ranges = [1, 5, 333]
         |      default-depth = 5
         |    }
         |    balance-watching-buffer-interval = 33s
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
         |          buffer-size = 100
         |          min-backoff = 11s
         |          max-backoff = 2d
         |        }
         |
         |        producer {
         |          enable = no
         |          buffer-size = 200
         |        }
         |      }
         |    }
         |    process-consumed-timeout = 663s
         |    $orderFeeStr
         |    $deviationsStr
         |    $allowedAssetPairsStr
         |    $orderRestrictionsStr
         |    $matchingRulesStr
         |    exchange-transaction-broadcast {
         |      broadcast-until-confirmed = yes
         |      interval = 1 day
         |      max-pending-time = 30 days
         |    }
         |  }
         |}""".stripMargin

    loadConfig(ConfigFactory.parseString(configStr))
  }
}
