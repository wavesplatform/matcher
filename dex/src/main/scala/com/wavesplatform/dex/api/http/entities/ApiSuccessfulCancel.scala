package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(
  description = "Successful cancellation message. Can be one of: ApiSuccessfulSingleCancel, ApiSuccessfulBatchCancel",
  subTypes = Array(
    classOf[ApiSuccessfulSingleCancel],
    classOf[ApiSuccessfulBatchCancel]
  )
)
class ApiSuccessfulCancel {

  @ApiModelProperty(value = "Success flag")
  val success: Boolean = ApiSuccessfulCancel.success

  @ApiModelProperty(allowableValues = "OrderCanceled, BatchCancelCompleted")
  val status: String = ApiSuccessfulCancel.status // @deprecated(message = "This field is unnecessary", since = "1.2.0")
}

object ApiSuccessfulCancel {
  val success: Boolean = true
  val status: String   = "OrderCanceled"
}
