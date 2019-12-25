package com.wavesplatform.dex.it.api.responses.dex

import com.wavesplatform.dex.util.getSimpleName
import play.api.libs.json.{Format, Reads, Writes}

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
