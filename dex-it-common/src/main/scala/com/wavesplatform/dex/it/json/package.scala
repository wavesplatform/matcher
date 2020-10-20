package com.wavesplatform.dex.it

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import im.mak.waves.transactions.{ExchangeTransaction, Transaction}
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

package object json {

  implicit val transactionFormat: Format[Transaction] = Format[Transaction](
    Reads { json =>
      Try(Transaction.fromJson(json.toString)) match {
        case Success(x) => JsSuccess(x)
        case Failure(e) => JsError(e.getMessage)
      }
    },
    Writes(tx => Json.parse(tx.toJson))
  )

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

  implicit val exchangeTxReads: Reads[ExchangeTransaction] = transactionFormat.map(_.asInstanceOf[ExchangeTransaction])

  implicit val orderWrites: Writes[Order] = Writes(_.json())

  implicit val assetPairFormat: Format[AssetPair] = AssetPair.assetPairFormat

  implicit val assetRatesReads: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { assetRates =>
      assetRates.map { case (assetStr, rateValue) => AssetPair.extractAsset(assetStr).get -> rateValue }
    }
  }

  implicit val assetBalancesReads: Reads[Map[Asset, Long]] = Reads.map[Long].map { assetBalances =>
    assetBalances.map { case (assetStr, balanceValue) => AssetPair.extractAsset(assetStr).get -> balanceValue }
  }

  implicit val assetPairOffsetsReads: Reads[Map[AssetPair, Long]] = Reads { json =>
    json.validate[Map[String, Long]].map {
      _.map {
        case (assetPairStr, offset) =>
          val assetPairStrArr = assetPairStr.split("-")
          val assetPair = (
            assetPairStrArr match {
              case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
              case _ => throw new Exception(s"$assetPairStr (incorrect assets count, expected 2 but got ${assetPairStrArr.size})")
            }
          ).fold(ex => throw new Exception(s"$assetPairStr (${ex.getMessage})"), identity)
          assetPair -> offset
      }
    }
  }

}
