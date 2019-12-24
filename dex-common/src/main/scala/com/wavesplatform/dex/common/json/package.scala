package com.wavesplatform.dex.common

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.{Asset, Transaction, TransactionFactory}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{Failure, Success}

package object json {
  implicit val assetFormat: Format[Asset] = Format(
    fjs = Reads {
      case JsNull | JsString("") => JsSuccess(Waves)
      case JsString(s) =>
        AssetPair.extractAssetId(s) match {
          case Failure(_)       => JsError(JsPath, JsonValidationError("error.incorrect.base58"))
          case Success(assetId) => JsSuccess(assetId)
        }
      case _ => JsError(JsPath, JsonValidationError("error.expected.jsstring"))
    },
    tjs = Writes(_.maybeBase58Repr.fold[JsValue](JsNull)(JsString))
  )

  implicit val assetRatesFormat: Format[Map[Asset, Double]]  = assetMapFormat[Double]
  implicit val assetBalancesFormat: Format[Map[Asset, Long]] = assetMapFormat[Long]

  implicit def assetMapFormat[V: Format]: Format[Map[Asset, V]] = mapFormat[Asset, V](
    stringifyKey = AssetPair.assetIdStr,
    parseKey = x => AssetPair.extractAssetId(x).fold(_ => JsError(s"Can't parse '$x' as Asset"), JsSuccess(_))
  )

  implicit val assetPairOffsetsFormat: Format[Map[AssetPair, Long]] = mapFormat[AssetPair, Long](
    stringifyKey = _.key,
    parseKey = x => {
      val assetPairStrArr = x.split("-")
      assetPairStrArr match {
        case Array(amtAssetStr, prcAssetStr) =>
          AssetPair
            .createAssetPair(amtAssetStr, prcAssetStr)
            .fold(e => JsError(s"Can't parse '$x' as AssetPair: ${e.getMessage}"), JsSuccess(_))
        case _ => JsError(s"$x (incorrect assets count, expected 2 but got ${assetPairStrArr.size})")
      }
    }
  )

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K]): Format[Map[K, V]] = {
    val vReads = implicitly[Reads[V]]
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) {
            case (r, (k, v)) =>
              for {
                r <- r
                k <- parseKey(k)
                v <- vReads.reads(v)
              } yield r.updated(k, v)
          }
        case x => JsError(s"Can't parse map: $x")
      },
      tjs = Writes.map[V].contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }

  implicit val byteStrFormat: Format[ByteStr] = Format(
    fjs = Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    tjs = Writes(x => JsString(x.toString))
  )

  implicit val assetPairFormat: Format[AssetPair] = (
    (JsPath \ "amountAsset").formatWithDefault[Asset](Waves)(assetFormat) and
      (JsPath \ "priceAsset").formatWithDefault[Asset](Waves)(assetFormat)
  )(AssetPair.apply, Function.unlift(AssetPair.unapply))

  implicit val transactionFormat: Format[Transaction] = Format[Transaction](
    json => TransactionFactory.fromSignedRequest(json).fold(e => JsError(s"Can't parse as transaction: $e, json: $json"), JsSuccess(_)),
    _.json()
  )

  implicit val exchangeTxReads: Reads[exchange.ExchangeTransaction] = Reads { json =>
    JsSuccess(TransactionFactory.fromSignedRequest(json).right.get.asInstanceOf[exchange.ExchangeTransaction])
  }

  implicit val orderWrites: Writes[Order] = Writes(_.json())
}
