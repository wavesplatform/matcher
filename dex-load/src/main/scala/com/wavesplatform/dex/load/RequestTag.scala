package com.wavesplatform.dex.load

import java.io.PrintWriter

import com.google.common.net.HttpHeaders
import com.wavesplatform.dex.load.RequestType.RequestType
import com.wavesplatform.dex.load.RequestTag.RequestTag
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.ApiJson

object RequestTag extends Enumeration {
  type RequestTag = Value
  val RESERVED_BALANCE      = Value("RESERVED_BALANCE")
  val TRADABLE_BALANCE      = Value("TRADABLE_BALANCE")
  val ORDER_BOOK = Value("ORDER_HISTORY_BY_PAIR")
  val ORDER_HISTORY_BY_ACC  = Value("ORDER_HISTORY_BY_ACC")
  val ORDER_STATUS          = Value("ORDER_STATUS")
  val CANCEL                = Value("CANCEL")
  val PLACE                 = Value("PLACE")
}

object RequestType extends Enumeration {
  type RequestType = Value
  val POST = Value("POST")
  val GET  = Value("GET")
}

case class Request(httpType: RequestType, path: String, tag: RequestTag, obj: ApiJson = null, headers: Map[String, String] = Map.empty) {
  val defaultHeaders = Map(
    HttpHeaders.ACCEPT       -> "application/json",
    HttpHeaders.CONNECTION   -> "close",
    HttpHeaders.CONTENT_TYPE -> "application/json",
    HttpHeaders.HOST         -> settings.hosts.shooted
  )

  def mkGet(path: String, tag: RequestTag, additionalHeaders: Map[String, String] = Map.empty) = {
    val request =
      s"${RequestType.GET} $path HTTP/1.1\r\n${(defaultHeaders ++ additionalHeaders).map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n"

    s"${request.length} $tag\n$request\r\n"
  }

  def mkPost(obj: ApiJson, path: String, tag: RequestTag): String = {
    val body = mkJson(obj).replace("\"matcherFeeAssetId\":\"WAVES\",", "")

    val headers = defaultHeaders ++ Map(
      HttpHeaders.CONTENT_LENGTH -> body.length.toString,
      "X-API-Key"                -> settings.dexRestApiKey
    )

    val request = s"${RequestType.POST} $path HTTP/1.1\r\n${headers.map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n$body"

    s"${request.length} $tag\n$request\r\n"
  }

  def save(pw: PrintWriter): Unit = {
    pw.println(httpType match {
      case RequestType.POST => mkPost(obj, path, tag)
      case RequestType.GET  => mkGet(path, tag, headers)
    })
  }
}
