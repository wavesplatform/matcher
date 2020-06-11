package com.wavesplatform.dex.load.request

import java.io.PrintWriter

import com.google.common.net.HttpHeaders
import com.wavesplatform.dex.load.request.RequestTag.RequestTag
import com.wavesplatform.dex.load.request.RequestType.RequestType
import com.wavesplatform.dex.load.utils.{mkJson, settings}
import com.wavesplatform.wavesj.ApiJson

case class Request(httpType: RequestType,
                   path: String,
                   tag: RequestTag,
                   jsonBody: ApiJson = null,
                   headers: Map[String, String] = Map.empty,
                   stringBody: String = "") {
  val defaultHeaders = Map(
    HttpHeaders.ACCEPT       -> "application/json",
    HttpHeaders.CONNECTION   -> "close",
    HttpHeaders.CONTENT_TYPE -> "application/json",
    HttpHeaders.HOST         -> settings.hosts.shooted
  )

  def mkGet(path: String, tag: RequestTag, additionalHeaders: Map[String, String] = Map.empty) = {
    val request =
      s"${RequestType.GET} $path HTTP/1.1\r\n${(defaultHeaders ++ additionalHeaders).map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n"

    s"${request.length} $tag\r\n$request\r\n"
  }

  def mkPost(obj: ApiJson, path: String, tag: RequestTag, stringBody: String = ""): String = {
    val body = if (stringBody.isEmpty) mkJson(obj).replace("\"matcherFeeAssetId\":\"WAVES\",", "") else stringBody

    val headers = defaultHeaders ++ Map(
      HttpHeaders.CONTENT_LENGTH -> body.length.toString,
      "X-API-Key"                -> settings.dexRestApiKey
    )

    val request = s"${RequestType.POST} $path HTTP/1.1\r\n${headers.map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n$body"

    s"${request.length} $tag\r\n$request\r\n"
  }

  def save(pw: PrintWriter): Unit = {
    pw.println(httpType match {
      case RequestType.POST => mkPost(jsonBody, path, tag, stringBody)
      case RequestType.GET  => mkGet(path, tag, headers)
    })
  }
}
