package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.{ApiModel, ApiModelProperty}

@ApiModel(
  description = "Successful cancellation message. Can be one of: HttpSuccessfulSingleCancel, HttpSuccessfulBatchCancel",
  subTypes = Array(
    classOf[HttpSuccessfulSingleCancel],
    classOf[HttpSuccessfulBatchCancel]
  )
)
class HttpSuccessfulCancel {

  @ApiModelProperty(value = "Success flag")
  val success: Boolean = HttpSuccessfulCancel.success

  @ApiModelProperty(allowableValues = "OrderCanceled, BatchCancelCompleted")
  val status: String = HttpSuccessfulCancel.status // @deprecated(message = "This field is unnecessary", since = "1.2.0")
}

object HttpSuccessfulCancel {
  val success: Boolean = true
  val status: String = "OrderCanceled"
}
