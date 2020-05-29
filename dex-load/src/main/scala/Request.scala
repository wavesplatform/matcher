package com.wavesplatform.dex.load

import java.io.PrintWriter

import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.ApiJson

case class Request(httpType: String, path: String, tag: String, obj: ApiJson = null, headers: Map[String, String] = Map.empty) {
  def save(pw: PrintWriter): Unit = httpType match {
    case "POST" => pw.println(mkPost(obj, path, tag))
    case _      => pw.println(mkGet(path, tag, headers))
  }
}
