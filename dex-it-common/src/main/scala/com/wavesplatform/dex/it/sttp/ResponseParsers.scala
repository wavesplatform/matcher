package com.wavesplatform.dex.it.sttp

import com.softwaremill.sttp.{DeserializationError, ResponseAs, MonadError => _, _}
import com.typesafe.config.{Config, ConfigFactory}
import play.api.libs.json.JsError

import scala.util.{Failure, Success, Try}

object ResponseParsers {

  val asUtf8String: ResponseAs[String, Nothing] = asString("UTF-8")

  def asConfig: ResponseAs[Either[DeserializationError[JsError], Config], Nothing] =
    asUtf8String.map { string =>
      Try(ConfigFactory.parseString(string)) match {
        case Success(r) => Right(r)
        case Failure(e) => Left(DeserializationError[JsError](string, JsError("Can't parse Config"), s"Can't parse Config: ${e.getMessage}"))
      }
    }
}
