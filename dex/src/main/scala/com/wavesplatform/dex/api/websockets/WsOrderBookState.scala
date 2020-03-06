package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.api.websockets.WsOrderBookState.WsSide
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

case class WsOrderBookState(asks: WsSide, bids: WsSide, lastTrade: WsLastTrade)
object WsOrderBookState {
  type WsSide = TreeMap[Double, Double]

  private implicit val doubleFormat = doubleAsStringFormat
  private val priceAmountFormat = Format(
    fjs = Reads.Tuple2R(doubleFormat, doubleFormat),
    tjs = Writes.Tuple2W(doubleFormat, doubleFormat)
  )

  private def sideFormat(pricesOrdering: Ordering[Double]): Format[WsSide] = Format(
    fjs = Reads {
      case JsArray(pairs) =>
        pairs.zipWithIndex.foldLeft[JsResult[WsSide]](JsSuccess(TreeMap.empty[Double, Double](pricesOrdering))) {
          case (r: JsError, _) => r

          case (JsSuccess(r, _), (pair, i)) =>
            for {
              (price, amount) <- pair.validate(priceAmountFormat)
              _               <- if (r.contains(price)) JsError(JsPath \ i \ 0, s"Side contains price $price twice") else JsSuccess(())
            } yield r.updated(price, amount)

          case (_, (x, i)) => JsError(JsPath \ i, "Can't read as price+amount pair")
        }
      case x => JsError(JsPath, s"Can't read Side from ${x.getClass.getName}")
    },
    tjs = Writes { xs =>
      Json.arr(xs.map(priceAmountFormat.writes))
    }
  )

  private val asksOrdering: Ordering[Double] = (x: Double, y: Double) => Ordering.Double.compare(x, y)
  private val bidsOrdering: Ordering[Double] = (x: Double, y: Double) => -Ordering.Double.compare(x, y)

  implicit val wsOrderBookStateFormat: Format[WsOrderBookState] = {
    ((JsPath \ "a").format[WsSide](sideFormat(asksOrdering)) and
      (JsPath \ "b").format[WsSide](sideFormat(bidsOrdering)) and
      (JsPath \ "t").format[WsLastTrade])(WsOrderBookState.apply, unlift(WsOrderBookState.unapply))
  }
}
