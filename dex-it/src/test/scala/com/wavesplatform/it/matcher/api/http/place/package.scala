package com.wavesplatform.it.matcher.api.http

import com.softwaremill.sttp.StatusCodes
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration.DurationInt

package object place extends MatcherSuiteBase with TableDrivenPropertyChecks {

  val correctExpiration = System.currentTimeMillis() + 10.days.toMillis

  val orderCases = Table(
    (
      "№",
      "Amount",
      "Price",
      "Fee",
      "Fee Asset",
      "Timestamp",
      "Duration",
      "Version",
      "Matcher Public Key",
      "Http status",
      "Error code",
      "Error Message"
    ),
    (
      1,
      -10.waves,
      1.usd,
      0.003.waves,
      Waves,
      correctExpiration,
      1.day,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9437184,
      "The order is invalid: amount should be > 0"
    ),
    (
      2,
      10.waves,
      -1.usd,
      0.003.waves,
      Waves,
      correctExpiration,
      1.day,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9437184,
      "The order is invalid: price should be > 0"
    ),
    (
      3,
      10.waves,
      1.usd,
      0L,
      Waves,
      correctExpiration,
      1.day,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9441542,
      "Required 0.003 WAVES as fee for this order, but given 0 WAVES"
    ),
    (
      4,
      10.waves,
      1.usd,
      0.003.waves,
      usd,
      correctExpiration,
      1.day,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9441540,
      s"Required one of the following fee asset: WAVES. But given ${UsdId.toString}"
    ),
    (
      5,
      10.waves,
      1.usd,
      0.003.waves,
      Waves,
      -1L,
      1.day,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9441798,
      "The expiration should be at least"
    ),
    (
      6,
      10.waves,
      1.usd,
      0.003.waves,
      Waves,
      correctExpiration,
      31.days,
      1.toByte,
      matcher.publicKey,
      StatusCodes.BadRequest,
      9437184,
      "The order is invalid: expiration should be earlier than 30 days"
    ),
    (
      7,
      10.waves,
      1.usd,
      0.003.waves,
      Waves,
      correctExpiration,
      1.day,
      1.toByte,
      alice.publicKey,
      StatusCodes.BadRequest,
      3076,
      s"The required matcher public key for this DEX is ${matcher.publicKey}, but given ${alice.publicKey}"
    )
  )

}
