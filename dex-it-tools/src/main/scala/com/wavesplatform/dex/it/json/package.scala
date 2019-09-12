package com.wavesplatform.dex.it

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.{Asset, Transaction, TransactionFactory}
import play.api.libs.json._

import scala.util.{Failure, Success}

package object json {
  implicit val byteStr: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val assetPair: Format[AssetPair] = Json.format[AssetPair]

  implicit val transaction: Format[Transaction] = Format[Transaction](
    json => JsSuccess(TransactionFactory.fromSignedRequest(json).explicitGet()),
    _.json()
  )

  implicit val assetDoubleMap: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { rate =>
      rate.map { case (assetStr, rateValue) => AssetPair.extractAssetId(assetStr).get -> rateValue }
    }
  }
}
