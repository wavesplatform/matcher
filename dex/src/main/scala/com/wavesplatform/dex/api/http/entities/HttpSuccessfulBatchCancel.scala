package com.wavesplatform.dex.api.http.entities

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

@ApiModel(description = "Cancel of multiple orders", parent = classOf[HttpSuccessfulCancel])
case class HttpSuccessfulBatchCancel(
                                      @ApiModelProperty(
      value = "List of successful cancellation messages or errors",
      dataType = "[[Lcom.wavesplatform.dex.api.http.entities.HttpSuccessfulSingleCancel;",
    )
    message: List[List[Either[HttpError, HttpSuccessfulSingleCancel]]], // TODO: In new API: should be a map id -> cancel result
                                      @ApiModelProperty(value = "Success flag")
    override val success: Boolean = HttpSuccessfulCancel.success,
                                      @ApiModelProperty(
      value = "Status",
      example = "BatchCancelCompleted",
      required = false
    ) override val status: String = "BatchCancelCompleted")
    extends HttpSuccessfulCancel

object HttpSuccessfulBatchCancel {

  implicit val httpSuccessfulBatchCancelFormat: Format[HttpSuccessfulBatchCancel] = {

    implicit val ef: Format[Either[HttpError, HttpSuccessfulSingleCancel]] =
      com.wavesplatform.dex.json.eitherFormat[HttpError, HttpSuccessfulSingleCancel]

    Json.format
  }

  def apply(message: List[Either[HttpError, HttpSuccessfulSingleCancel]]): HttpSuccessfulBatchCancel =
    HttpSuccessfulBatchCancel(message = List(message))
}
