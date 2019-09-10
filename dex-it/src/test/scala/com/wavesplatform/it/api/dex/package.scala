package com.wavesplatform.it.api

import cats.MonadError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.util.getSimpleName
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import play.api.libs.json._

import scala.util.{Failure, Success}

package object dex {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]
}

sealed abstract class OrderStatus(val isActive: Boolean) extends Product with Serializable {
  val name = getSimpleName(this)
}

object OrderStatus {

  case object Accepted        extends OrderStatus(true)
  case object NotFound        extends OrderStatus(false)
  case object PartiallyFilled extends OrderStatus(true)
  case object Filled          extends OrderStatus(false)
  case object Cancelled       extends OrderStatus(false)

  val All = List(Accepted, NotFound, PartiallyFilled, Filled, Cancelled)

  implicit val format: Format[OrderStatus] = Format(
    Reads.StringReads.map { x =>
      All.find(_.name == x) match {
        case Some(r) => r
        case None    => throw new IllegalArgumentException(s"Can't parse '$x' as OrderStatus")
      }
    },
    Writes.StringWrites.contramap(_.name)
  )
}

case class OrderBookHistoryItem(id: Order.Id,
                                `type`: String,
                                amount: Long,
                                price: Long,
                                timestamp: Long,
                                filled: Int,
                                status: OrderStatus,
                                assetPair: AssetPair)

object OrderBookHistoryItem {
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

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]

  implicit val orderbookHistory: Format[OrderBookHistoryItem] = Json.format
}
