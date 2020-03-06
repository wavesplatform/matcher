package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.api.websockets.WsOrderBook.WsSide
import com.wavesplatform.dex.domain.model.{Amount, Denormalization, Price}
import com.wavesplatform.dex.fp.MayBeEmpty
import com.wavesplatform.dex.json.Implicits.JsPathOps
import com.wavesplatform.dex.model.{LastTrade, LevelAgg, LevelAmounts}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

case class WsOrderBook private (asks: WsSide, bids: WsSide, lastTrade: Option[WsLastTrade]) {
  def nonEmpty: Boolean = asks.nonEmpty || bids.nonEmpty || lastTrade.nonEmpty
}

object WsOrderBook {
  type WsSide = TreeMap[Double, Double]

  private val asksOrdering: Ordering[Double] = (x: Double, y: Double) => Ordering.Double.compare(x, y)
  private val bidsOrdering: Ordering[Double] = (x: Double, y: Double) => -Ordering.Double.compare(x, y)

  val empty = WsOrderBook(
    asks = TreeMap.empty(asksOrdering),
    bids = TreeMap.empty(bidsOrdering),
    lastTrade = None
  )

  implicit val wsOrderBookStateFormat: Format[WsOrderBook] =
    ((JsPath \ "a").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (JsPath \ "b").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (JsPath \ "t").formatNullable[WsLastTrade])(WsOrderBook.apply, unlift(WsOrderBook.unapply))

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

  class Update(amountAssetDecimals: Int, priceAssetDecimals: Int) {
    def from(asks: Iterable[LevelAgg], bids: Iterable[LevelAgg], lt: Option[LastTrade]): WsOrderBook = WsOrderBook(
      asks = side(asks, asksOrdering),
      bids = side(bids, bidsOrdering),
      lastTrade = lt.map(lastTrade)
    )

    def lastTrade(x: LastTrade): WsLastTrade = WsLastTrade(
      price = Denormalization.denormalizePrice(x.price, amountAssetDecimals, priceAssetDecimals).toDouble,
      amount = Denormalization.denormalizeAmountAndFee(x.amount, amountAssetDecimals).toDouble,
      side = x.side
    )

    def side(xs: Iterable[LevelAgg], ordering: Ordering[Double]): WsSide =
      xs.map { x =>
        Denormalization.denormalizePrice(x.price, amountAssetDecimals, priceAssetDecimals).toDouble ->
          Denormalization.denormalizeAmountAndFee(x.amount, amountAssetDecimals).toDouble
      }(collection.breakOut)

    def withLevelChanges(orig: WsOrderBook, updated: LevelAmounts): WsOrderBook = orig.copy(
      asks = orig.asks ++ denormalized(updated.asks),
      bids = orig.bids ++ denormalized(updated.bids)
    )

    def withLastTrade(orig: WsOrderBook, x: LastTrade): WsOrderBook = orig.copy(lastTrade = Some(lastTrade(x)))

    private def denormalized(xs: Map[Price, Amount]): Map[Double, Double] = xs.map {
      case (price, amount) =>
        Denormalization.denormalizePrice(price, amountAssetDecimals, priceAssetDecimals).toDouble ->
          Denormalization.denormalizeAmountAndFee(amount, amountAssetDecimals).toDouble
    }
  }
}
