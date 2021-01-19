package com.wavesplatform.it.matcher.api.http.balances

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetTradableBalanceSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/tradableBalance/{address}" - {
    "should return current asset balances if user doesn't have opened orders" in {
      val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
      validate200Json(dex1.rawApi.getTradableBalance(acc, wavesUsdPair)) should be(Map(Waves -> 100.waves, usd -> 50.usd))
    }

    "should return correct balance value after user place order" in {
      val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
      List(
        mkOrder(acc, wavesUsdPair, BUY, 2.waves, 1.usd),
        mkOrder(acc, wavesUsdPair, SELL, 1.waves, 5.usd)
      ).foreach(placeAndAwaitAtDex(_))

      validate200Json(dex1.rawApi.getTradableBalance(acc, wavesUsdPair)) should be(Map(
        Waves -> 98.994.waves, // waves = 100 - 1 - 2 * 0.003
        usd -> 48.usd // usd = 50 - 1 * 2
      ))

      withClue(" and should recalculate it after match") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 5.usd))
        validate200Json(dex1.rawApi.getTradableBalance(acc, wavesUsdPair)) should be(Map(
          Waves -> 98.994.waves,
          usd -> 53.usd
        ))
      }

      withClue(" and should free it after cancel") {
        dex1.api.cancelAll(acc)
        eventually {
          validate200Json(dex1.rawApi.getTradableBalance(acc, wavesUsdPair)) should be(Map(
            Waves -> 98.997.waves,
            usd -> 55.usd
          ))
        }
      }
    }

    "should return an error exception when the amount asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getTradableBalance(alice.toAddress.stringRepr, "null", UsdId.toString),
        StatusCodes.BadRequest,
        11534337,
        s"The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error exception when the price asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getTradableBalance(alice.toAddress.stringRepr, "WAVES", "null"),
        StatusCodes.BadRequest,
        11534337,
        s"The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error when amount asset doesn't exist" in {
      val incorrectAsset = "3Q6ndEq2z5UJwF4SF24ySRj9guPoFWaSeXP"
      validateMatcherError(
        dex1.rawApi.getTradableBalance(alice.toAddress.stringRepr, incorrectAsset, "WAVES"),
        StatusCodes.NotFound,
        11534345,
        s"The asset $incorrectAsset not found"
      )
    }

    //TODO:  DEX-981 | incorrect error, because of asset 3Q6ndEq2z5UJwF4SF24ySRj9guPoFWaSeXP doesn't exist
    "should return an error when price asset doesn't exist" in {
      val incorrectAsset = "3Q6ndEq2z5UJwF4SF24ySRj9guPoFWaSeXP"
      validateMatcherError(
        dex1.rawApi.getTradableBalance(alice.toAddress.stringRepr, "WAVES", incorrectAsset),
        StatusCodes.NotFound,
        9440771,
        s"The WAVES-$incorrectAsset asset pair should be reversed"
      )
    }

    "should return redirect when amount and price assets is not in correct order" in {
      validate301Redirect(dex1.rawApi.getTradableBalance(alice.toAddress.stringRepr, UsdId.toString, "WAVES"))
    }

    "should return an error if address has a bad checksum" in {
      validateMatcherError(
        dex1.rawApi.getTradableBalance("3Q6ndEq2z5UJwFaSF24ySRj9guPoFWaSeXX", "WAVES", UsdId.toString),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Bad address checksum"
      )
    }

    "should return an error if address has an incorrect length" in {
      validateMatcherError(
        dex1.rawApi.getTradableBalance("AAAAA", "WAVES", UsdId.toString),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Wrong addressBytes length: expected: 26, actual: 4"
      )
    }

    "should return an error if address is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getTradableBalance("null", "WAVES", UsdId.toString),
        StatusCodes.BadRequest,
        4194304,
        "Provided address in not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }
  }
}
