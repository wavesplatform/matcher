package com.wavesplatform.dex.api.ws.entities

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.doubleAsStringFormat
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.utils.JsonImplicits._
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrder(
  id: Order.Id,
  timestamp: Option[Long],
  amountAsset: Asset,
  priceAsset: Asset,
  side: OrderType,
  isMarket: Option[Boolean],
  price: Option[Double],
  amount: Option[Double],
  fee: Option[Double],
  feeAsset: Asset,
  status: Option[String],
  filledAmount: Option[Double],
  filledFee: Option[Double],
  avgWeighedPrice: Option[Double],
  totalExecutedPriceAssets: Option[Double],
  matchInfo: Seq[WsMatchTransactionInfo]
)

object WsOrder {

  def apply(
    id: Order.Id,
    amountAsset: Asset,
    priceAsset: Asset,
    feeAsset: Asset,
    side: OrderType,
    timestamp: Option[Long] = None,
    isMarket: Option[Boolean] = None,
    price: Option[Double] = None,
    amount: Option[Double] = None,
    fee: Option[Double] = None,
    status: Option[String] = None,
    filledAmount: Option[Double] = None,
    filledFee: Option[Double] = None,
    avgWeighedPrice: Option[Double] = None,
    totalExecutedPriceAssets: Option[Double] = None,
    matchInfo: Seq[WsMatchTransactionInfo] = Seq.empty
  ): WsOrder = new WsOrder(
    id = id,
    amountAsset = amountAsset,
    priceAsset = priceAsset,
    side = side,
    timestamp = timestamp,
    isMarket = isMarket,
    price = price,
    amount = amount,
    fee = fee,
    feeAsset = feeAsset,
    status = status,
    filledAmount = filledAmount,
    filledFee = filledFee,
    avgWeighedPrice = avgWeighedPrice,
    totalExecutedPriceAssets = totalExecutedPriceAssets,
    matchInfo = matchInfo
  )

  def fromOrder(
    order: Order,
    timestamp: Option[Long] = None,
    isMarket: Option[Boolean] = None,
    price: Option[Double] = None,
    amount: Option[Double] = None,
    fee: Option[Double] = None,
    status: Option[String] = None,
    filledAmount: Option[Double] = None,
    filledFee: Option[Double] = None,
    avgWeighedPrice: Option[Double] = None,
    totalExecutedPriceAssets: Option[Double] = None,
    matchInfo: Seq[WsMatchTransactionInfo] = Seq.empty
  ): WsOrder = WsOrder(
    order.id(),
    timestamp,
    order.assetPair.amountAsset,
    order.assetPair.priceAsset,
    order.orderType,
    isMarket,
    price,
    amount,
    fee,
    order.feeAsset,
    status,
    filledAmount,
    filledFee,
    avgWeighedPrice,
    totalExecutedPriceAssets,
    matchInfo
  )

  val isMarketFormat: Format[Boolean] = AcceptedOrderType.acceptedOrderTypeFormat.coerce[Boolean](
    { case AcceptedOrderType.Market => true; case _ => false },
    if (_) AcceptedOrderType.Market else AcceptedOrderType.Limit
  )

  implicit class JsPathOps(private val path: JsPath) {

    val formatAsset: OFormat[Asset] = OFormat(
      Reads[Asset] { json =>
        path.asSingleJson(json) match {
          case JsDefined(JsNull) => JsSuccess(Waves)
          case JsDefined(value) => Asset.assetFormat.reads(value)
          case _ => throw new IllegalArgumentException(s"Can't process json=$json")
        }
      },
      Writes.at[Asset](path)(Asset.assetFormat.writes _)
    )

  }

  implicit val wsOrderFormat: Format[WsOrder] =
    (
      (__ \ "i").format[Order.Id] and // id
        (__ \ "t").formatNullable[Long] and // timestamp
        (__ \ "A").formatAsset and // amount asset
        (__ \ "P").formatAsset and // price asset
        (__ \ "S").format[OrderType] and // side: buy or sell
        (__ \ "T").formatNullable[Boolean](isMarketFormat) and // type: market or limit
        (__ \ "p").formatNullable[Double](doubleAsStringFormat) and // price
        (__ \ "a").formatNullable[Double](doubleAsStringFormat) and // amount
        (__ \ "f").formatNullable[Double](doubleAsStringFormat) and // fee
        (__ \ "F").formatAsset and // fee asset
        (__ \ "s").formatNullable[String] and // status: Accepted or Filled or PartiallyFilled or Cancelled
        (__ \ "q").formatNullable[Double](doubleAsStringFormat) and // filled amount
        (__ \ "Q").formatNullable[Double](doubleAsStringFormat) and // filled fee
        (__ \ "r").formatNullable[Double](doubleAsStringFormat) and // average weighed price among all trades
        (__ \ "E").formatNullable[Double](doubleAsStringFormat) and // total executed price assets
        (__ \ "m").formatNullable[Seq[WsMatchTransactionInfo]].inmap[Seq[WsMatchTransactionInfo]](
          o => o.getOrElse(Seq.empty[WsMatchTransactionInfo]),
          s => if (s.isEmpty) None else Some(s)
        ) // match transaction information (such as executed asset amount and etc)
    )(WsOrder.apply, unlift(WsOrder.unapply))

}
