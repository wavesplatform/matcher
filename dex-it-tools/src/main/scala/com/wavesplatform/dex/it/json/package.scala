package com.wavesplatform.dex.it

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.{Asset, Transaction, TransactionFactory}
import play.api.libs.json._

import scala.util.{Failure, Success}

package object json {

  implicit val byteStrFormat: Format[ByteStr] = Format(
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

  implicit val transactionFormat: Format[Transaction] = Format[Transaction](
    json => JsSuccess { TransactionFactory.fromSignedRequest(json).explicitGet() },
    _.json()
  )

  // TODO
  implicit val exchangeTxReads: Reads[exchange.ExchangeTransaction] = Reads { json =>
    JsSuccess(TransactionFactory.fromSignedRequest(json).right.get.asInstanceOf[exchange.ExchangeTransaction])
  }

  implicit val orderWrites: Writes[Order] = Writes(_.json())

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]

  implicit val assetRatesReads: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { assetRates =>
      assetRates.map { case (assetStr, rateValue) => AssetPair.extractAssetId(assetStr).get -> rateValue }
    }
  }

  implicit val assetBalancesReads: Reads[Map[Asset, Long]] = Reads.map[Long].map { assetBalances =>
    assetBalances.map { case (assetStr, balanceValue) => AssetPair.extractAssetId(assetStr).get -> balanceValue }
  }

  implicit val assetPairOffsetsReads: Reads[Map[AssetPair, Long]] = Reads { json =>
    json.validate[Map[String, Long]].map {
      _.map {
        case (assetPairStr, offset) =>
          val assetPairStrArr = assetPairStr.split("-")
          val assetPair = (
            assetPairStrArr match {
              case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
              case _                               => throw new Exception(s"$assetPairStr (incorrect assets count, expected 2 but got ${assetPairStrArr.size})")
            }
          ) fold (ex => throw new Exception(s"$assetPairStr (${ex.getMessage})"), identity)
          assetPair -> offset
      }
    }
  }
}
