package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.{Marshaller, PredefinedToEntityMarshallers, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshaller}
import akka.util.ByteString
import com.wavesplatform.dex.api.http.json.CustomJson
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import play.api.libs.json._

import scala.util.control.Exception.nonFatalCatch
import scala.util.control.NoStackTrace

case class PlayJsonException(cause: Option[Throwable] = None, errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty)
    extends IllegalArgumentException(s"JSON parsing errors:\n${errors.mkString("\n")}", cause.orNull)
    with NoStackTrace

trait ApiMarshallers {

  implicit lazy val ExchangeTransactionJsonWrites: Writes[ExchangeTransaction] = Writes(_.json())

  private[this] lazy val jsonStringUnmarshaller =
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset)       => data.decodeString(charset.nioCharset.name)
      }

  private[this] lazy val jsonStringMarshaller = Marshaller.stringMarshaller(`application/json`)

  private[this] lazy val customJsonStringMarshaller = Marshaller.stringMarshaller(CustomJson.jsonWithNumbersAsStrings)

  implicit def playJsonUnmarshaller[A](implicit reads: Reads[A]): FromEntityUnmarshaller[A] =
    jsonStringUnmarshaller.map { data =>
      val json = nonFatalCatch.withApply(t => throw PlayJsonException(cause = Some(t)))(Json.parse(data))

      json.validate[A] match {
        case JsSuccess(value, _) => value
        case JsError(errors)     => throw PlayJsonException(errors = errors.map { case (jp, errorsSeq) => jp -> errorsSeq.to(Seq) } to Seq)
      }
    }

  // preserve support for extracting plain strings from requests
  implicit val stringUnmarshaller: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller
  implicit val intUnmarshaller: FromEntityUnmarshaller[Int]       = stringUnmarshaller.map(_.toInt)

  implicit def playJsonMarshaller[A](implicit writes: Writes[A], jsValueToString: JsValue => String = Json.stringify): ToEntityMarshaller[A] =
    Marshaller.oneOf(
      jsonStringMarshaller
        .compose(jsValueToString)
        .compose(writes.writes),
      customJsonStringMarshaller
        .compose(CustomJson.writeValueAsString)
        .compose(writes.writes)
    )

  // preserve support for using plain strings as request entities
  implicit val stringMarshaller = PredefinedToEntityMarshallers.stringMarshaller(`text/plain`)
}

object ApiMarshallers extends ApiMarshallers
