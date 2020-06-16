package com.wavesplatform.dex.api.websockets

import cats.data.NonEmptyList
import cats.syntax.option._
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.AcceptedOrder
import com.wavesplatform.dex.model.Events.{ExchangeTransactionCreated, OrderCanceled}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrdersUpdate(orders: NonEmptyList[WsCompleteOrder], timestamp: Long = System.currentTimeMillis) extends WsServerMessage {
  override val tpe: String = WsOrdersUpdate.tpe

  def append(other: WsOrdersUpdate): WsOrdersUpdate = copy(
    orders = other.orders ::: orders,
    timestamp = other.timestamp
  )
}

object WsOrdersUpdate {

  val tpe = "osu"

  def from(x: OrderCanceled)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = WsOrdersUpdate(
    NonEmptyList.one(WsCompleteOrder.from(x))
  )

  def from(x: ExchangeTransactionCreated)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = {
    val ao1       = x.reason.counter
    val assetPair = ao1.order.assetPair

    val amountAssetDecimals = efc.assetDecimals(assetPair.amountAsset)
    val priceAssetDecimals  = efc.assetDecimals(assetPair.priceAsset)

    def denormalizeAmountAndFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double        = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble
    def from(ao: AcceptedOrder): WsCompleteOrder     = WsCompleteOrder.from(ao, x.reason, denormalizeAmountAndFee, denormalizePrice)

    WsOrdersUpdate(NonEmptyList.of(ao1, x.reason.submitted).map(from), timestamp = x.tx.timestamp)
  }

  def wsUnapply(arg: WsOrdersUpdate): Option[(String, Long, NonEmptyList[WsCompleteOrder])] = (arg.tpe, arg.timestamp, arg.orders).some

  implicit def nonEmptyListFormat[T: Format]: Format[NonEmptyList[T]] = Format(
    Reads.list[T].flatMap { xs =>
      NonEmptyList.fromList(xs).fold[Reads[NonEmptyList[T]]](Reads.failed("The list is empty"))(Reads.pure(_))
    },
    Writes.list[T].contramap(_.toList)
  )

  implicit val wsOrdersUpdateFormat: Format[WsOrdersUpdate] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "o").format(nonEmptyListFormat[WsCompleteOrder])
  )(
    (_, ts, orders) => WsOrdersUpdate(orders, ts),
    unlift(WsOrdersUpdate.wsUnapply)
  )
}
