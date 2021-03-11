package com.wavesplatform.dex.it.sttp

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import play.api.libs.json.JsError
import sttp.client3._
import sttp.client3.playJson._

import scala.util.{Failure, Success, Try}

object ResponseParsers {

  val asUtf8String: ResponseAs[Either[String, String], Nothing] = asString("UTF-8")

  def asLong: ResponseAs[Either[DeserializationException[JsError], Long], Nothing] =
    asUtf8String.map {
      case Left(l) => Right(1L) // TODO: ???
      case Right(r) =>
        val l = Longs.tryParse(r)
        if (l == null) Left(DeserializationException[JsError](r, JsError("Can't parse Long")))
        else Right(l)
    }

  def asConfig: ResponseAs[Either[DeserializationException[JsError], Config], Nothing] =
    asUtf8String.map {
      string =>
        Try(ConfigFactory.parseString(string.getOrElse(""))) match {
          case Success(r) => Right(r)
          case Failure(e) => Left(DeserializationException[JsError](string.getOrElse(""), JsError("Can't parse Config")))
        }
    }

}
