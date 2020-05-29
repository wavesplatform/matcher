package com.wavesplatform.dex.load

import java.io.PrintWriter

import com.wavesplatform.dex.load.utils.mkPost
import com.wavesplatform.wavesj.ApiJson

case class Request(obj: ApiJson, httpType: String, path: String, tag: String) {
  def save(pw: PrintWriter): Unit = httpType match {
    case "POST" => pw.println(mkPost(obj, path, tag))
    case _      => pw.println("get")
  }
}
