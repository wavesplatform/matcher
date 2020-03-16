package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.api.websockets.WsOrderBook.WsSide
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.fp.MayBeEmpty
import com.wavesplatform.dex.json.Implicits.JsPathOps
import com.wavesplatform.dex.model.{LastTrade, LevelAgg, LevelAmounts}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

case class WsOrderBook(asks: WsSide, bids: WsSide, lastTrade: Option[WsLastTrade], timestamp: Long = System.currentTimeMillis) {
  def nonEmpty: Boolean = asks.nonEmpty || bids.nonEmpty || lastTrade.nonEmpty
}

object WsOrderBook {
  type WsSide = TreeMap[Double, Double]

  private val asksOrdering: Ordering[Double] = (x: Double, y: Double) => Ordering.Double.compare(x, y)
  private val bidsOrdering: Ordering[Double] = (x: Double, y: Double) => -Ordering.Double.compare(x, y)

  val empty: WsOrderBook =
    WsOrderBook(
      asks = TreeMap.empty(asksOrdering),
      bids = TreeMap.empty(bidsOrdering),
      lastTrade = None
    )

  implicit val wsOrderBookStateFormat: Format[WsOrderBook] =
    ((__ \ "a").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (__ \ "b").formatMayBeEmpty[WsSide](sideFormat(bidsOrdering), sideMayBeEmpty(bidsOrdering)) and
      (__ \ "t").formatNullable[WsLastTrade] and
      (__ \ "_").format[Long])(WsOrderBook.apply, unlift(WsOrderBook.unapply))

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

          case (_, (_, i)) => JsError(JsPath \ i, "Can't read as price+amount pair")
        }
      case x => JsError(JsPath, s"Can't read Side from ${x.getClass.getName}")
    },
    tjs = Writes { xs =>
      JsArray(xs.map(priceAmountFormat.writes)(collection.breakOut))
    }
  )

  private def sideMayBeEmpty(ordering: Ordering[Double]): MayBeEmpty[WsSide] = new MayBeEmpty[WsSide] {
    override def isEmpty(x: WsSide): Boolean = x.isEmpty
    override def empty: WsSide               = TreeMap.empty(ordering)
  }

  class Update(amountAssetDecimals: Int, priceAssetDecimals: Int) {
    def from(asks: Iterable[LevelAgg], bids: Iterable[LevelAgg], lt: Option[LastTrade]): WsOrderBook =
      WsOrderBook(asks = side(asks, asksOrdering), bids = side(bids, bidsOrdering), lastTrade = lt.map(lastTrade))

    def lastTrade(x: LastTrade): WsLastTrade = WsLastTrade(
      price = denormalizePrice(x.price, amountAssetDecimals, priceAssetDecimals).toDouble,
      amount = denormalizeAmountAndFee(x.amount, amountAssetDecimals).toDouble,
      side = x.side
    )

    def side(xs: Iterable[LevelAgg], ordering: Ordering[Double]): WsSide =
      TreeMap(
        xs.map { x =>
          denormalizePrice(x.price, amountAssetDecimals, priceAssetDecimals).toDouble ->
            denormalizeAmountAndFee(x.amount, amountAssetDecimals).toDouble
        }.toSeq: _*
      )(ordering)

    def withLevelChanges(orig: WsOrderBook, updated: LevelAmounts): WsOrderBook = orig.copy(
      asks = orig.asks ++ denormalized(updated.asks),
      bids = orig.bids ++ denormalized(updated.bids),
      timestamp = System.currentTimeMillis
    )

    def withLastTrade(orig: WsOrderBook, x: LastTrade): WsOrderBook =
      orig.copy(lastTrade = Some(lastTrade(x)), timestamp = System.currentTimeMillis)

    private def denormalized(xs: Map[Price, Amount]): Map[Double, Double] = xs.map {
      case (price, amount) =>
        denormalizePrice(price, amountAssetDecimals, priceAssetDecimals).toDouble ->
          denormalizeAmountAndFee(amount, amountAssetDecimals).toDouble
    }
  }
}
