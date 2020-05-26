package com.wavesplatform.dex.load

import com.google.common.net.HttpHeaders
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.wavesj.ApiJson
import com.wavesplatform.wavesj.json.WavesJsonMapper

package object utils extends ScorexLogging {

  def waitForHeightArise(env: Environment): Unit = {
    val h = env.node.getHeight
    while (env.node.getHeight < h + 1) Thread.sleep(5000)
  }

  def getJson(env: Environment, obj: ApiJson): String = new WavesJsonMapper(env.networkByte).writeValueAsString(obj)

  def postRequest(env: Environment, obj: ApiJson, path: String, tag: String = "") = {
    val body = getJson(env, obj)

    val headers = Map(
      HttpHeaders.HOST           -> env.loadHost,
      HttpHeaders.ACCEPT         -> "application/json",
      HttpHeaders.CONNECTION     -> "close",
      "X-API-Key"                -> env.apiKey,
      HttpHeaders.CONTENT_LENGTH -> body.length.toString,
      HttpHeaders.CONTENT_TYPE   -> "application/json"
    )

    val request = s"POST $path HTTP/1.1\r\n${headers.mkString("", "\r\n", "").replace("->", ":")}\r\n\r\n$body"

    s"${request.length} ${tag.toUpperCase}\n$request\r\n"
  }
}
