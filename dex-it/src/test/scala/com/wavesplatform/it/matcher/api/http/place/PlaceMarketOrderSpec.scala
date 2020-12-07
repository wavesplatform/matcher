package com.wavesplatform.it.matcher.api.http.place

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpSuccessfulPlace
import com.wavesplatform.dex.domain.account
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.Json

import scala.concurrent.duration.Duration

class PlaceMarketOrderSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "POST /matcher/orderbook/market" - {

    "should place order" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

      validate200Json(dex1.rawApi.placeMarket(o)) should matchTo(HttpSuccessfulPlace(o))
    }

    "should return error with incorrect order signature" in {
      validateMatcherErrorContainText(
        dex1.rawApi.placeMarket(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd).json().deepMerge(Json.obj("amount" -> 3.waves))),
        StatusCodes.BadRequest,
        9440512,
        s"The signature of order"
      )
    }

    forAll(orderCases) { (n: Int, a: Long, p: Long, f: Long, fa: Asset, t: Long, d: Duration, v: Byte, pk: account.PublicKey, c: Int, e: Int, m: String) =>
      s"For row $n should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherErrorContainText(dex1.rawApi.placeMarket(mkOrder(alice, wavesUsdPair, BUY, a, p, f, fa, t, d, v, pk)), c, e, m)
      }
    }
  }
}
