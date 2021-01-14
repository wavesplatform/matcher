package com.wavesplatform.it.matcher.api.http.balances

import com.google.common.primitives.Longs
import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks
import org.scalatest.prop.TableDrivenPropertyChecks

class GetReservedBalanceSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with ApiKeyHeaderChecks {

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

  "GET /matcher/balance/reserved/{publicKey}" - {
    "should return empty object if account doesn't have opened orders" in {
      validate200Json(dex1.rawApi.getReservedBalance(alice))
    }

    "should return non-zero balances for opened orders" in {
      val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
      List(
        mkOrder(acc, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(acc, wavesUsdPair, SELL, 8.waves, 4.usd),
        mkOrder(acc, wavesUsdPair, SELL, 1.waves, 5.usd)
      ).foreach(placeAndAwaitAtDex(_))

      validate200Json(dex1.rawApi.getReservedBalance(acc)) should be(Map(Waves -> 9.009.waves, usd -> 20.usd))
    }

    "should return non-zero balances with X-API-KEY" in {
      val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
      List(
        mkOrder(acc, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(acc, wavesUsdPair, SELL, 8.waves, 4.usd),
        mkOrder(acc, wavesUsdPair, SELL, 1.waves, 5.usd)
      ).foreach(placeAndAwaitAtDex(_))

      validate200Json(dex1.rawApi.getReservedBalanceWithApiKey(acc)) should be(Map(Waves -> 9.009.waves, usd -> 20.usd))
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), headers = Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), incorrectApiKeyHeader))

    "should return an error if publicKey is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getReservedBalance("null", System.currentTimeMillis, "sign"),
        StatusCodes.BadRequest,
        3148801,
        "Provided public key is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return an error if publicKey parameter has the different value of used in signature" in {
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(bob.publicKey), ts, sign))
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val ts = System.currentTimeMillis
      val sign = Base58.encode(crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts)))

      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), ts + 1000, sign))
    }

    "should return an error with incorrect signature" in {
      validateIncorrectSignature(dex1.rawApi.getReservedBalance(Base58.encode(alice.publicKey), System.currentTimeMillis, "incorrect"))
    }
  }
}
