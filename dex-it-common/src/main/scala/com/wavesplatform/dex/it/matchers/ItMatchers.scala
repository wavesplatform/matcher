package com.wavesplatform.dex.it.matchers

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import org.scalatest.matchers.Matcher

import scala.Ordered._

trait ItMatchers {

  def failWith(errorCode: Int): Matcher[Either[MatcherError, Any]]                      = new FailWith(errorCode)
  def failWith(errorCode: Int, messagePart: String): Matcher[Either[MatcherError, Any]] = new FailWith(errorCode, Some(messagePart))

  def failWith(errorCode: Int, containsParams: MatcherError.Params): Matcher[Either[MatcherError, Any]] = {
    new FailWith(errorCode, None, containsParams)
  }

  def failWithBalanceNotEnough(required: Map[Asset, Long] = Map.empty, available: Map[Asset, Long] = Map.empty)(
      implicit assetDecimalsMap: Map[Asset, Int]): Matcher[Either[MatcherError, Any]] = {

    def stringifyBalances(map: Map[Asset, Long]): String = {
      map.toList
        .sortWith { (l, r) =>
          l._1.compatId < r._1.compatId
        }
        .map { case (a, b) => s"${Denormalization.denormalizeAmountAndFee(b, assetDecimalsMap(a))} ${a.toString}" }
        .mkString(" and ")
    }

    lazy val requiredStr  = stringifyBalances(required)
    lazy val availableStr = stringifyBalances(available)

    val errorMsg =
      if (required.isEmpty) availableStr
      else s"Not enough tradable balance. The order requires at least $requiredStr on balance, but available are $availableStr"

    failWith(3147270, errorMsg)
  }
}
