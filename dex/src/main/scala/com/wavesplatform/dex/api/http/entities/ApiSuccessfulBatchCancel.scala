package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

@ApiModel(description = "Cancel of multiple orders", parent = classOf[ApiSuccessfulCancel])
case class ApiSuccessfulBatchCancel(
    @ApiModelProperty(
      value = "List of successful cancellation messages or errors",
      dataType = "[[Lcom.wavesplatform.dex.api.http.entities.ApiSuccessfulSingleCancel;",
    )
    message: List[List[Either[ApiError, ApiSuccessfulSingleCancel]]], // TODO: In new API: should be a map id -> cancel result
    @ApiModelProperty(value = "Success flag")
    override val success: Boolean = ApiSuccessfulCancel.success,
    @ApiModelProperty(
      value = "Status",
      example = "BatchCancelCompleted",
      required = false
    ) override val status: String = "BatchCancelCompleted")
    extends ApiSuccessfulCancel

object ApiSuccessfulBatchCancel {

  implicit val apiSuccessfulBatchCancelFormat: Format[ApiSuccessfulBatchCancel] = {

    implicit val ef: Format[Either[ApiError, ApiSuccessfulSingleCancel]] =
      com.wavesplatform.dex.json.eitherFormat[ApiError, ApiSuccessfulSingleCancel]

    Json.format
  }

  def apply(message: List[Either[ApiError, ApiSuccessfulSingleCancel]]): ApiSuccessfulBatchCancel = ApiSuccessfulBatchCancel(message = List(message))
}
