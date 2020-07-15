package com.wavesplatform.dex.api.ws.protocol

import cats.syntax.option._
import com.wavesplatform.dex.api.ws._
import com.wavesplatform.dex.api.ws.entities.{WsLastTrade, WsOrderBookSettings}
import com.wavesplatform.dex.api.ws.protocol.WsOrderBookChanges.WsSide
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.fp.MayBeEmpty
import com.wavesplatform.dex.json.Implicits.JsPathOps
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.immutable.TreeMap

case class WsOrderBookChanges(assetPair: AssetPair,
                              asks: WsSide,
                              bids: WsSide,
                              lastTrade: Option[WsLastTrade],
                              updateId: Long,
                              settings: Option[WsOrderBookSettings],
                              timestamp: Long = System.currentTimeMillis)
    extends WsServerMessage {
  override val tpe: String = WsOrderBookChanges.tpe
}

object WsOrderBookChanges {

  val tpe = "ob"

  def wsUnapply(arg: WsOrderBookChanges): Option[(String, AssetPair, Long, Long, WsSide, WsSide, Option[WsLastTrade], Option[WsOrderBookSettings])] =
    (arg.tpe, arg.assetPair, arg.timestamp, arg.updateId, arg.asks, arg.bids, arg.lastTrade, arg.settings).some

  type WsSide = TreeMap[Double, Double]

  private val asksOrdering: Ordering[Double] = (x: Double, y: Double) => Ordering.Double.IeeeOrdering.compare(x, y)
  private val bidsOrdering: Ordering[Double] = (x: Double, y: Double) => -Ordering.Double.IeeeOrdering.compare(x, y)

  def empty(assetPair: AssetPair): WsOrderBookChanges =
    WsOrderBookChanges(
      assetPair = assetPair,
      asks = TreeMap.empty(asksOrdering),
      bids = TreeMap.empty(bidsOrdering),
      lastTrade = None,
      settings = None,
      updateId = 0
    )

  implicit val wsOrderBookChangesFormat: Format[WsOrderBookChanges] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[AssetPair](AssetPair.assetPairKeyAsStringFormat) and
      (__ \ "_").format[Long] and
      (__ \ "U").format[Long] and
      (__ \ "a").formatMayBeEmpty[WsSide](sideFormat(asksOrdering), sideMayBeEmpty(asksOrdering)) and
      (__ \ "b").formatMayBeEmpty[WsSide](sideFormat(bidsOrdering), sideMayBeEmpty(bidsOrdering)) and
      (__ \ "t").formatNullable[WsLastTrade] and
      (__ \ "s").formatNullable[WsOrderBookSettings]
  )(
    (_, assetPair, timestamp, uid, asks, bids, lastTrade, settings) => WsOrderBookChanges(assetPair, asks, bids, lastTrade, uid, settings, timestamp),
    unlift(WsOrderBookChanges.wsUnapply)
  )

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
      JsArray(xs.map(priceAmountFormat.writes).to(Seq))
    }
  )

  private def sideMayBeEmpty(ordering: Ordering[Double]): MayBeEmpty[WsSide] = new MayBeEmpty[WsSide] {
    override def isEmpty(x: WsSide): Boolean = x.isEmpty
    override def empty: WsSide               = TreeMap.empty(ordering)
  }

  def from(assetPair: AssetPair,
           amountDecimals: Int,
           priceDecimals: Int,
           asks: Iterable[LevelAgg],
           bids: Iterable[LevelAgg],
           lt: Option[LastTrade],
           updateId: Long,
           restrictions: Option[OrderRestrictionsSettings],
           tickSize: Double): WsOrderBookChanges =
    WsOrderBookChanges(
      assetPair = assetPair,
      asks = side(amountDecimals, priceDecimals, asks, asksOrdering),
      bids = side(amountDecimals, priceDecimals, bids, bidsOrdering),
      lastTrade = lt.map(lastTrade(amountDecimals, priceDecimals, _)),
      updateId = updateId,
      settings = WsOrderBookSettings(restrictions, tickSize.some).some
    )

  def lastTrade(amountDecimals: Int, priceDecimals: Int, x: LastTrade): WsLastTrade = WsLastTrade(
    price = denormalizePrice(x.price, amountDecimals, priceDecimals).toDouble,
    amount = denormalizeAmountAndFee(x.amount, amountDecimals).toDouble,
    side = x.side
  )

  def side(amountDecimals: Int, priceDecimals: Int, xs: Iterable[LevelAgg], ordering: Ordering[Double]): WsSide =
    TreeMap(
      xs.map { x =>
        denormalizePrice(x.price, amountDecimals, priceDecimals).toDouble ->
          denormalizeAmountAndFee(x.amount, amountDecimals).toDouble
      }.toSeq: _*
    )(ordering)
}
