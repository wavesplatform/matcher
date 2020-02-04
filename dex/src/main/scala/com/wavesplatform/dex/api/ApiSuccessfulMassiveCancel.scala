package com.wavesplatform.dex.api

import play.api.libs.json.{Format, JsError, Json, Reads, Writes}

import scala.reflect.ClassTag

case class ApiSuccessfulMassiveCancel(
    // TODO: In new API: should be a map id -> cancel result
    message: List[List[Either[ApiError, ApiSuccessfulCancel]]],
    success: Boolean = true,
    status: String = "BatchCancelCompleted"
)

object ApiSuccessfulMassiveCancel {
  implicit def eitherFormat[L, R](implicit lFormat: Format[L], rFormat: Format[R], ctl: ClassTag[L], ctr: ClassTag[R]): Format[Either[L, R]] = Format(
    Reads { js =>
      js.validate[R]
        .map(Right[L, R])
        .orElse {
          js.validate[L].map(Left[L, R])
        }
        .orElse(JsError(s"Can't parse as Either[${ctl.runtimeClass.getName}, ${ctr.runtimeClass.getName}]"))
    },
    Writes {
      case Right(x) => rFormat.writes(x)
      case Left(x)  => lFormat.writes(x)
    }
  )

  implicit val apiSuccessfulMassiveCancelFormat: Format[ApiSuccessfulMassiveCancel] = Json.format

  def apply(message: List[Either[ApiError, ApiSuccessfulCancel]]): ApiSuccessfulMassiveCancel = ApiSuccessfulMassiveCancel(message = List(message))
}
