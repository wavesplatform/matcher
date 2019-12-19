package com.wavesplatform.it.api

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._

import scala.util.{Failure, Success}

case class OrderInfo(id: String,
                     version: Option[Byte],
                     sender: String,
                     senderPublicKey: String,
                     matcherPublicKey: String,
                     assetPair: AssetPairResponse,
                     orderType: String,
                     amount: Long,
                     price: Long,
                     timestamp: Long,
                     expiration: Long,
                     matcherFee: Long,
                     signature: String,
                     proofs: Option[Seq[String]])

object OrderInfo {
  implicit val transactionFormat: Format[OrderInfo] = Json.format
}

case class AssetPairResponse(amountAsset: Option[String], priceAsset: Option[String])

object AssetPairResponse {
  implicit val pairResponseFormat: Format[AssetPairResponse] = Json.format
}

case class MatchingRules(tickSize: String)
object MatchingRules {
  implicit val matchingRules: Format[MatchingRules] = Json.format
}

case class OrderRestrictions(minAmount: String,
                             maxAmount: String,
                             stepAmount: String,
                             minPrice: String,
                             maxPrice: String,
                             stepPrice: String)
object OrderRestrictions {
  implicit val orderRestrictions: Format[OrderRestrictions] = Json.format
}

// TODO rename entities named "MatcherSomething" to "Something" after entities from model of node will be relocated
case class MatcherOrderbookInfo(matchingRules: MatchingRules,
                                restrictions: Option[OrderRestrictions])

object MatcherOrderbookInfo {
  implicit val orderbookInfo: Format[MatcherOrderbookInfo] = Json.format
}

case class StateChangesDetails(data: Seq[DataResponse], transfers: Seq[TransfersInfoResponse])

object StateChangesDetails {
  implicit val stateChangeResponseFormat: Format[StateChangesDetails] = Json.format[StateChangesDetails]
}

case class DebugStateChanges(`type`: Int,
                             id: String,
                             fee: Long,
                             timestamp: Long,
                             sender: Option[String],
                             height: Int,
                             minSponsoredAssetFee: Option[Long],
                             recipient: Option[String],
                             script: Option[String],
                             stateChanges: Option[StateChangesDetails]) extends TxInfo

object DebugStateChanges {
  implicit val debugStateChanges: Format[DebugStateChanges] = Json.format
}

case class DataResponse(`type`: String, value: Long, key: String)

object DataResponse {
  implicit val dataResponseFormat: Format[DataResponse] = Json.format
}

case class TransfersInfoResponse(address: String, asset: Option[String], amount: Long)

object TransfersInfoResponse {
  implicit val assetIdReads: Reads[Option[String]] = Reads {
    case JsString(str) => JsSuccess(Some(str))
    case JsNull => JsSuccess(None)
    case _ => JsError("Unexpected value")
  }

  implicit val transfersInfoResponseFormat: Format[TransfersInfoResponse] = Json.format
}

case class ExchangeTransaction(`type`: Int,
                               version: Option[Byte],
                               id: String,
                               sender: String,
                               senderPublicKey: String,
                               fee: Long,
                               timestamp: Long,
                               proofs: Option[Seq[String]],
                               order1: OrderInfo,
                               order2: OrderInfo,
                               amount: Long,
                               price: Long,
                               buyMatcherFee: Long,
                               sellMatcherFee: Long,
                               height: Option[Int])

object ExchangeTransaction {
  implicit val transactionFormat: Format[ExchangeTransaction] = Json.format
}

case class Block(signature: String,
                 height: Int,
                 timestamp: Long,
                 generator: String,
                 transactions: Seq[Transaction],
                 fee: Long,
                 features: Option[Seq[Short]],
                 reward: Option[Long])

case class MatcherMessage(id: String)

object MatcherMessage {
  implicit val matcherMessageFormat: Format[MatcherMessage] = Json.format
}

case class MatcherResponse(status: String, message: MatcherMessage)

object MatcherResponse {
  implicit val matcherResponseFormat: Format[MatcherResponse] = Json.format
}

case class RatesResponse(message: String)

object RatesResponse {
  implicit val format: Format[RatesResponse] = Json.format
}

case class MatcherErrorResponse(status: Option[String], message: Option[String])

object MatcherErrorResponse {
  implicit val matcherErrorResponseFormat: Format[MatcherErrorResponse] = Json.format
}

case class MarketDataInfo(matcherPublicKey: String, markets: Seq[MarketData])

object MarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MarketDataInfo] = Json.format
}

case class AssetDecimalsInfo(decimals: Byte)

object AssetDecimalsInfo {
  implicit val assetDecimalsInfoResponseFormat: Format[AssetDecimalsInfo] = Json.format
}

case class MarketData(amountAsset: String,
                      amountAssetName: String,
                      priceAsset: String,
                      priceAssetName: String,
                      created: Long,
                      amountAssetInfo: Option[AssetDecimalsInfo],
                      priceAssetInfo: Option[AssetDecimalsInfo],
                      matchingRules: MatchingRules,
                      restrictions: Option[OrderRestrictions])

object MarketData {
  implicit val marketData: Format[MarketData] = Json.format
}

//TODO remove after merge with DEX-355
case class MatcherStatusResponseWithFee(status: String, filledAmount: Option[Long], filledFee: Option[Long])

object MatcherStatusResponseWithFee {
  implicit val matcherStatusResponseWithFeeFormat: Format[MatcherStatusResponseWithFee] = Json.format
}

case class MessageMatcherResponse(message: String)

object MessageMatcherResponse {
  implicit val messageMatcherResponseFormat: Format[MessageMatcherResponse] = Json.format
}

case class OrderHistory(id: String,
                        `type`: String,
                        amount: Long,
                        fee: Long,
                        price: Long,
                        timestamp: Long,
                        filled: Long,
                        filledFee: Long,
                        feeAsset: Asset,
                        status: String,
                        assetPair: AssetPair,
                        orderType: String) {
  def isActive: Boolean = status == "PartiallyFilled" || status == "Accepted"
}

object OrderHistory {
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

  implicit val orderbookHistory: Format[OrderHistory] = Json.format
}

case class PairResponse(amountAsset: String, priceAsset: String)

object PairResponse {
  implicit val pairResponseFormat: Format[PairResponse] = Json.format
}

case class LevelResponse(amount: Long, price: Long)

object LevelResponse {
  implicit val levelResponseFormat: Format[LevelResponse] = Json.format
}

case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])

object OrderBookResponse {
  implicit val orderBookResponseFormat: Format[OrderBookResponse] = Json.format
}

case class MarketStatusResponse(lastPrice: Option[Long],
                                lastSide: Option[String],
                                bid: Option[Long],
                                bidAmount: Option[Long],
                                ask: Option[Long],
                                askAmount: Option[Long])

object MarketStatusResponse {
  implicit val marketResponseFormat: Format[MarketStatusResponse] = Json.format
}