package com.wavesplatform.it.api

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{Format, JsError, JsString, JsSuccess, Json, Reads, Writes}

import scala.util.{Failure, Success}

case class MatcherStatusResponse(status: String, filledAmount: Option[Long], filledFee: Option[Long])
object MatcherStatusResponse {
  implicit val matcherStatusResponseFormat: Format[MatcherStatusResponse] = Json.format
}

case class OrderbookHistory(id: String,
                            `type`: String,
                            amount: Long,
                            fee: Long,
                            price: Long,
                            timestamp: Long,
                            filled: Long,
                            filledFee: Long,
                            feeAsset: Asset,
                            status: String,
                            assetPair: AssetPair) {
  def isActive: Boolean = status == "PartiallyFilled" || status == "Accepted"
}
object OrderbookHistory {
  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    Writes(x => JsString(x.base58))
  )

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]

  implicit val orderbookHistory: Format[OrderbookHistory] = Json.format
}