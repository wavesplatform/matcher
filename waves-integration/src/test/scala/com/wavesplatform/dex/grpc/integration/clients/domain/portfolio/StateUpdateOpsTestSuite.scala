package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits.StateUpdateOps
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.Transaction
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scalapb.json4s.JsonFormat

class StateUpdateOpsTestSuite extends WavesIntegrationSuiteBase with PBEntitiesGen with ScalaCheckDrivenPropertyChecks with NoShrink {

  private val negativeChangesAddress, address1, address2 = mkPBAddress
  private val asset = mkPBIssuedAsset

  "StateUpdateOps" - {
    "pessimisticPortfolio" - {
      "counts only spending" in {
        val changes = List(
          (address1, pbWaves, 10L),
          (address1, asset, 0L),
          (negativeChangesAddress, pbWaves, -20L),
          (negativeChangesAddress, asset, -21L)
        )

        val update = StateUpdate(
          balances = changes.map { case (address, asset, v) =>
            StateUpdate.BalanceUpdate(
              address = address,
              amount = Amount(asset, v).some
            )
          }
        )

        update.pessimisticPortfolios(none) should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(
            Asset.Waves -> -20L,
            asset.toVanillaAsset -> -21L
          )
        ))
      }

      "counts outgoing leasing" in {
        val changes = List(
          (address1, -10L),
          (address2, 0L),
          (negativeChangesAddress, 21L)
        )

        val update = StateUpdate(
          leases = changes.map { case (address, v) =>
            StateUpdate.LeasingUpdate(
              address = address,
              out = v
            )
          }
        )

        update.pessimisticPortfolios(none) should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(Asset.Waves -> -21L)
        ))
      }

      "sum out leases and spending for Waves" in {
        val balanceUpdateChanges = List(
          (address1, pbWaves, 10L),
          (address1, asset, 0L),
          (negativeChangesAddress, pbWaves, -20L),
          (negativeChangesAddress, asset, -21L)
        )

        val leasingChanges = List(
          (address1, -10L),
          (address2, 0L),
          (negativeChangesAddress, 22L)
        )

        val update = StateUpdate(
          balances = balanceUpdateChanges.map { case (address, asset, v) =>
            StateUpdate.BalanceUpdate(
              address = address,
              amount = Amount(asset, v).some
            )
          },
          leases = leasingChanges.map { case (address, v) =>
            StateUpdate.LeasingUpdate(
              address = address,
              out = v
            )
          }
        )

        update.pessimisticPortfolios(none) should matchTo(Map[Address, Map[Asset, Long]](
          negativeChangesAddress.toVanillaAddress -> Map(
            Asset.Waves -> -42L,
            asset.toVanillaAsset -> -21L
          )
        ))
      }

      "exchange transactions" - {
        val alice = Address.fromBytes(ByteStr.decodeBase58("3N68Ssvfab5zDG8nE3zJCr4JuhGRHWBAqyQ").get).explicitGet()
        val bob = Address.fromBytes(ByteStr.decodeBase58("3N6DiZg75XDUxnNGHYjzsCMFkSY9Qr22W3K").get).explicitGet()

        "one trader" in {
          val aliceVsAliceTxJson = """{
  "chainId": 84,
  "senderPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
  "fee": {
    "amount": "300000"
  },
  "timestamp": "1612279893539",
  "version": 2,
  "exchange": {
    "amount": "300000000",
    "price": "200",
    "buyMatcherFee": "300000",
    "sellMatcherFee": "300000",
    "orders": [
      {
        "chainId": 84,
        "senderPublicKey": "v9+YGXWTywCXMq+5JoZdhdGKQqWXO2D07qyh4HLR+FA=",
        "matcherPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
        "assetPair": {
          "priceAssetId": "6yPcNL9avQO/uCZ6JD0DF9RORsdkmR6q6BBHYXgBaIM="
        },
        "amount": "300000000",
        "price": "200",
        "timestamp": "1612279893540",
        "expiration": "1614871892540",
        "matcherFee": {
          "amount": "300000"
        },
        "version": 1,
        "proofs": [
          "QlpZCiScsjDKY7Y5NMMtXz8mF5MrRvxa+L4UkmUQIitYkBgzqHFv1vAc3vQtTFRU/U4/PHKgjRIqfN+NJMBjhg=="
        ]
      },
      {
        "chainId": 84,
        "senderPublicKey": "v9+YGXWTywCXMq+5JoZdhdGKQqWXO2D07qyh4HLR+FA=",
        "matcherPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
        "assetPair": {
          "priceAssetId": "6yPcNL9avQO/uCZ6JD0DF9RORsdkmR6q6BBHYXgBaIM="
        },
        "orderSide": "SELL",
        "amount": "300000000",
        "price": "200",
        "timestamp": "1612279893557",
        "expiration": "1614871892557",
        "matcherFee": {
          "amount": "300000"
        },
        "version": 3,
        "proofs": [
          "zgo/Dn2RtAU4Xxe5FbygB2+d+K6d+M9lxV7Z3MbWqpMSjknbj9TbKAh5xCURe/naXvogHBnLzFQFW6m9ckFPjg=="
        ]
      }
    ]
  }
}"""
          val tx = JsonFormat.fromJsonString[Transaction](aliceVsAliceTxJson).getExchange
          val usd = IssuedAsset(ByteStr.decodeBase58("GptXPPw96xEGjtwK6rN7wi6RtJN2rxN26DRVGVZnwoZC").get)

          Implicits.exchangeTransactionPessimisticPortfolios(tx) should matchTo(Map(
            alice -> Map[Asset, Long](
              Asset.Waves -> -(3_00000000 + 2 * 300_000), // sell 3 WAVES and spend fees for both orders
              usd -> -3 * 200 // buy 3 WAVES for 200 cents each
            )
          ))
        }

        "different traders" in {
          val aliceVsBobTxJson = """{
  "chainId": 84,
  "senderPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
  "fee": {
    "amount": "300000"
  },
  "timestamp": "1612280151130",
  "version": 2,
  "exchange": {
    "amount": "300000000",
    "price": "200",
    "buyMatcherFee": "300000",
    "sellMatcherFee": "300000",
    "orders": [
      {
        "chainId": 84,
        "senderPublicKey": "v9+YGXWTywCXMq+5JoZdhdGKQqWXO2D07qyh4HLR+FA=",
        "matcherPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
        "assetPair": {
          "priceAssetId": "b0KUdpo/mnF9aAZZoHVt/jKeEsLYgNHUB/hBAR9ozSw="
        },
        "amount": "300000000",
        "price": "200",
        "timestamp": "1612280151132",
        "expiration": "1614872150132",
        "matcherFee": {
          "amount": "300000"
        },
        "version": 3,
        "proofs": [
          "QK1XqXvb1BPb7nw/gEP2E/Ire+4nVrUIyYuAP/JU6T59aBe1Zp/d7QyZsHgzVPL6+yxm9KKzBl1FH3hvpfgShw=="
        ]
      },
      {
        "chainId": 84,
        "senderPublicKey": "4yIwFXOsQBp6QXLUlIgznonWf/ocrh85ag04xDO8PFk=",
        "matcherPublicKey": "XdsW0Tpqk/mGWGot+4VH+c6Uvnz+qsHGIvfaGIlp01Q=",
        "assetPair": {
          "priceAssetId": "b0KUdpo/mnF9aAZZoHVt/jKeEsLYgNHUB/hBAR9ozSw="
        },
        "orderSide": "SELL",
        "amount": "300000000",
        "price": "200",
        "timestamp": "1612280151149",
        "expiration": "1614872150149",
        "matcherFee": {
          "amount": "300000"
        },
        "version": 3,
        "proofs": [
          "j2Aul3xLAtAw/Y5rSn49J3VumBzjJwlWPNzLdKX6ZrvjD5PqbgaO6dIk/VphYRZpBodlZZpOYNxP4CJRk/OKgQ=="
        ]
      }
    ]
  }
}"""
          val tx = JsonFormat.fromJsonString[Transaction](aliceVsBobTxJson).getExchange
          val usd = IssuedAsset(ByteStr.decodeBase58("8VK91Wi3LrLoSFamB1aeXJVfArpaH5iJi2wA69fyNGmH").get)

          Implicits.exchangeTransactionPessimisticPortfolios(tx) should matchTo(Map(
            bob -> Map[Asset, Long](
              Asset.Waves -> -(3_00000000 + 300_000), // sell 3 WAVES and spend fee
            ),
            alice -> Map[Asset, Long](
              Asset.Waves -> -300_000, // spend fee
              usd -> -3 * 200 // buy 3 WAVES for 200 cents each
            )
          ))
        }
      }

      "properties" - {
        "only negative changes" in forAll(pbStateUpdateGen) { update =>
          update.pessimisticPortfolios(none).foreach { case (address, assets) =>
            withClue(s"$address: ") {
              assets.foreach { case (asset, v) =>
                withClue(s"$asset: ") {
                  v should be < 0L
                }
              }
            }
          }
        }
      }
    }
  }

}
