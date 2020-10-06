package com.wavesplatform.dex.it.sttp

import com.google.common.primitives.Longs
import com.softwaremill.sttp.{DeserializationError, ResponseAs, MonadError => _, _}
import com.typesafe.config.{Config, ConfigFactory}
import play.api.libs.json.JsError

import scala.util.{Failure, Success, Try}

object ResponseParsers {

  val asUtf8String: ResponseAs[String, Nothing] = asString("UTF-8")

  def asLong: ResponseAs[Either[DeserializationError[JsError], Long], Nothing] =
    asUtf8String.map { string =>
      val r = Longs.tryParse(string)
      if (r == null) Left(DeserializationError[JsError](string, JsError("Can't parse Long"), "Can't parse Long"))
      else Right(r)
    }

  def asConfig: ResponseAs[Either[DeserializationError[JsError], Config], Nothing] =
    asUtf8String.map { string =>
      Try(ConfigFactory.parseString(string)) match {
        case Success(r) => Right(r)
        case Failure(e) => Left(DeserializationError[JsError](string, JsError("Can't parse Config"), s"Can't parse Config: ${e.getMessage}"))
      }
    }

}
