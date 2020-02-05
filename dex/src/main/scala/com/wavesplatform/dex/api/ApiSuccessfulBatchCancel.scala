package com.wavesplatform.dex.api

import play.api.libs.json.{Format, Json}

case class ApiSuccessfulBatchCancel(
    // TODO: In new API: should be a map id -> cancel result
    message: List[List[Either[ApiError, ApiSuccessfulCancel]]],
    success: Boolean = true,
    status: String = "BatchCancelCompleted"
)

object ApiSuccessfulBatchCancel {
  implicit val apiSuccessfulBatchCancelFormat: Format[ApiSuccessfulBatchCancel] = {
    implicit val ef: Format[Either[ApiError, ApiSuccessfulCancel]] = com.wavesplatform.dex.json.eitherFormat[ApiError, ApiSuccessfulCancel]
    Json.format
  }

  def apply(message: List[Either[ApiError, ApiSuccessfulCancel]]): ApiSuccessfulBatchCancel = ApiSuccessfulBatchCancel(message = List(message))
}
