package com.wavesplatform.dex.api

import com.github.ghik.silencer.silent
import io.swagger.annotations.{ApiModel, ApiModelProperty}

@silent("deprecated")
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

  @deprecated(message = "This field is unnecessary", since = "1.2.0")
  @ApiModelProperty(allowableValues = "OrderCanceled, BatchCancelCompleted")
  val status: String = ApiSuccessfulCancel.status
}

object ApiSuccessfulCancel {
  val success: Boolean = true
  val status: String   = "OrderCanceled"
}
