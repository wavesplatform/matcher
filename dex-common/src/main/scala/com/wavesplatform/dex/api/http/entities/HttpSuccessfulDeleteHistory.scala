package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.Order
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}

@ApiModel(
  description = "Successful deletion message. Can be HttpSuccessfulDeleteHistory",
  subTypes = Array(
    classOf[HttpSuccessfulDeleteHistory]
  )
)
class HttpSuccessfulDelete {

  @ApiModelProperty(value = "Success flag")
  val success: Boolean = HttpSuccessfulDelete.success

  @ApiModelProperty(allowableValues = "OrderDeleted")
  val status: String = HttpSuccessfulDelete.status

}

object HttpSuccessfulDelete {
  val success: Boolean = true
  val status: String = "OrderDeleted"
}

@ApiModel(description = "Delete the single order from history", parent = classOf[HttpSuccessfulDelete])
case class HttpSuccessfulDeleteHistory(
  @ApiModelProperty(
    value = "Base58 encoded Order ID",
    dataType = "string",
    example = "7VEr4T9icqopHWLawGAZ7AQiJbjAcnzXn65ekYvbpwnN"
  ) orderId: Order.Id,
  @ApiModelProperty(value = "Success flag")
  override val success: Boolean = HttpSuccessfulDelete.success,
  @ApiModelProperty(
    value = "Status",
    example = "OrderDeleted",
    required = false
  ) override val status: String = "OrderDeleted"
) extends HttpSuccessfulCancel

object HttpSuccessfulDeleteHistory {
  implicit val httpSuccessfulDeleteHistoryFormat: OFormat[HttpSuccessfulDeleteHistory] = Json.format[HttpSuccessfulDeleteHistory]
}
