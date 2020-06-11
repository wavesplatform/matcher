package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.order.Order
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json._

@ApiModel(description = "Cancel of the single order", parent = classOf[ApiSuccessfulCancel])
case class ApiSuccessfulSingleCancel(@ApiModelProperty(
                                       value = "Base58 encoded Order ID",
                                       dataType = "string",
                                       example = "7VEr4T9icqopHWLawGAZ7AQiJbjAcnzXn65ekYvbpwnN"
                                     ) orderId: Order.Id,
                                     @ApiModelProperty(value = "Success flag")
                                     override val success: Boolean = ApiSuccessfulCancel.success,
                                     @ApiModelProperty(
                                       value = "Status",
                                       example = "OrderCanceled",
                                       required = false
                                     ) override val status: String = "OrderCanceled")
    extends ApiSuccessfulCancel

object ApiSuccessfulSingleCancel {
  implicit val apiSuccessfulCancelFormat: OFormat[ApiSuccessfulSingleCancel] = Json.format[ApiSuccessfulSingleCancel]
}
