package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws._
import com.wavesplatform.dex.domain.bytes.ByteStr
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsMatchTransactionInfo(
  txId: ByteStr,
  timestamp: Long,
  price: Double,
  executedAmountAssets: Double,
  executedPriceAssets: Double
)

object WsMatchTransactionInfo {

  implicit val wsMatchTransactionFormat: Format[WsMatchTransactionInfo] =
    (
      (__ \ "i").format[ByteStr] and // id
        (__ \ "t").format[Long] and // timestamp
        (__ \ "p").format[Double](doubleAsStringFormat) and // match execution price
        (__ \ "A").format[Double](doubleAsStringFormat) and // amount asset quantity executed in match
        (__ \ "P").format[Double](doubleAsStringFormat) // price asset quantity executed in match
    )(WsMatchTransactionInfo.apply, unlift(WsMatchTransactionInfo.unapply))

}
