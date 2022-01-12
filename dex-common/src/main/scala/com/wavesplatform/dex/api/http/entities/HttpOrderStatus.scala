package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.meta.getSimpleName
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, Reads, Writes}

case class HttpOrderStatus(
  @ApiModelProperty(
    dataType = "string",
    allowableValues = "Accepted, NotFound, PartiallyFilled, Filled, Cancelled",
    required = true
  ) status: Status,
  @ApiModelProperty(
    value = "Filled amount of existed order",
    dataType = "integer"
  ) filledAmount: Option[Long] = None,
  @ApiModelProperty(
    value = "Filled fee of existed order",
    dataType = "integer"
  ) filledFee: Option[Long] = None,
  @ApiModelProperty(
    value = "Brief message in case of not existed order"
  ) message: Option[String] = None
)

object HttpOrderStatus {

  implicit val httpOrderStatusFormat: Format[HttpOrderStatus] = Json.format

  sealed abstract class Status extends Product with Serializable {
    val name: String = getSimpleName(this)
  }

  object Status {

    case object Accepted extends Status
    case object NotFound extends Status
    case object PartiallyFilled extends Status
    case object Filled extends Status
    case object Cancelled extends Status

    val All = List(Accepted, NotFound, PartiallyFilled, Filled, Cancelled)

    implicit val format: Format[Status] = Format(
      Reads.StringReads.map { x =>
        All.find(_.name == x) match {
          case Some(r) => r
          case None => throw new IllegalArgumentException(s"Can't parse '$x' as ApiOrderStatus.Status")
        }
      },
      Writes.StringWrites.contramap(_.name)
    )

  }

}
