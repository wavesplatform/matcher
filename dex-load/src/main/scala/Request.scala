package com.wavesplatform.dex.load

import java.io.PrintWriter

import com.google.common.net.HttpHeaders
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.ApiJson

case class Request(httpType: String, path: String, tag: String, obj: ApiJson = null, headers: Map[String, String] = Map.empty) {
  def mkGet(path: String, tag: String = "", additionalHeaders: Map[String, String] = Map.empty) = {
    val request = s"GET $path HTTP/1.1\r\n${(defaultHeaders ++ additionalHeaders).map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n"

    s"${request.length} ${tag.toUpperCase}\n$request\r\n"
  }

  def mkPost(obj: ApiJson, path: String, tag: String = ""): String = {
    val body = mkJson(obj).replace("\"matcherFeeAssetId\":\"WAVES\",", "")

    val headers = defaultHeaders ++ Map(
      HttpHeaders.HOST           -> settings.loadHost,
      HttpHeaders.CONTENT_LENGTH -> body.length.toString,
      "X-API-Key"                -> settings.apiKey
    )

    val request = s"POST $path HTTP/1.1\r\n${headers.map { case (k, v) => s"$k: $v" }.mkString("\r\n")}\r\n\r\n$body"

    s"${request.length} ${tag.toUpperCase}\n$request\r\n"
  }

  def save(pw: PrintWriter): Unit = httpType match {
    case "POST" => pw.println(mkPost(obj, path, tag))
    case _      => pw.println(mkGet(path, tag, headers))
  }
}
