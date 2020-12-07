package com.wavesplatform.it.matcher.api.http.place

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.api.responses.dex.MatcherError.Params
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration.DurationInt

class PlaceOrderBaseSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  val correctExpiration = System.currentTimeMillis() + 10.days.toMillis

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

  val orderCases = Table(
    (
      "№",
      "Order",
      "Status",
      "Message"
    ),
    (
      1,
      mkOrder(alice, wavesUsdPair, BUY, -10.waves, 1.usd, 0.003.waves, Waves, correctExpiration, 1.day, 1.toByte, matcher.publicKey),
      StatusCodes.BadRequest,
      MatcherError(9437184, "The order is invalid: amount should be > 0", "OrderRejected", Some(Params(None, None, None)))
    ),
    (
      2,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, -1.usd, 0.003.waves, Waves, correctExpiration, 1.day, 1.toByte, matcher.publicKey),
      StatusCodes.BadRequest,
      MatcherError(9437184, "The order is invalid: price should be > 0", "OrderRejected", Some(Params(None, None, None)))
    ),
    (
      3,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd, 0L, Waves, correctExpiration, 1.day, 1.toByte, matcher.publicKey),
      StatusCodes.BadRequest,
      MatcherError(9441542, "Required 0.003 WAVES as fee for this order, but given 0 WAVES", "OrderRejected", Some(Params(None, None, None)))
    ),
    (
      4,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd, 0.003.waves, usd, correctExpiration, 1.day, 1.toByte, matcher.publicKey),
      400,
      MatcherError(
        9441540,
        s"Required one of the following fee asset: WAVES. But given ${UsdId.toString}",
        "OrderRejected",
        Some(Params(None, None, None))
      )
    ),
    (
      5,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd, 0.003.waves, Waves, -1L, 1.day, 1.toByte, matcher.publicKey),
      StatusCodes.BadRequest,
      MatcherError(9441798, "The expiration should be at least", "OrderRejected", Some(Params(None, None, None)))
    ),
    (
      6,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd, 0.003.waves, Waves, correctExpiration, 31.days, 1.toByte, matcher.publicKey),
      StatusCodes.BadRequest,
      MatcherError(9437184, "The order is invalid: expiration should be earlier than 30 days", "OrderRejected", Some(Params(None, None, None)))
    ),
    (
      7,
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd, 0.003.waves, Waves, correctExpiration, 1.day, 1.toByte, alice.publicKey),
      StatusCodes.BadRequest,
      MatcherError(
        3076,
        s"The required matcher public key for this DEX is ${matcher.publicKey}, but given ${alice.publicKey}",
        "OrderRejected",
        Some(Params(None, None, None))
      )
    )
  )

}
