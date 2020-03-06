package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.api.websockets.WsOrderBookState.WsSide
import com.wavesplatform.dex.fp.MayBeEmpty
import com.wavesplatform.dex.json.Implicits.JsPathOps
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

case class WsOrderBookState private (asks: WsSide, bids: WsSide, lastTrade: Option[WsLastTrade])
object WsOrderBookState {
  type WsSide = TreeMap[Double, Double]

  private val asksOrdering: Ordering[Double] = (x: Double, y: Double) => Ordering.Double.compare(x, y)
  private val bidsOrdering: Ordering[Double] = (x: Double, y: Double) => -Ordering.Double.compare(x, y)

  val empty = WsOrderBookState(
    asks = TreeMap.empty(asksOrdering),
    bids = TreeMap.empty(bidsOrdering),
    lastTrade = None
  )

  implicit val wsOrderBookStateFormat: Format[WsOrderBookState] =
    ((JsPath \ "a").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (JsPath \ "b").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (JsPath \ "t").formatNullable[WsLastTrade])(WsOrderBookState.apply, unlift(WsOrderBookState.unapply))

  private val priceAmountFormat = Format(
    fjs = Reads.Tuple2R(doubleAsStringFormat, doubleAsStringFormat),
    tjs = Writes.Tuple2W(doubleAsStringFormat, doubleAsStringFormat)
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

  private def sideMayBeEmpty(ordering: Ordering[Double]): MayBeEmpty[WsSide] = new MayBeEmpty[WsSide] {
    override def isEmpty(x: WsSide): Boolean = x.isEmpty
    override def empty: WsSide               = TreeMap.empty
  }
}
