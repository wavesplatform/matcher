package com.wavesplatform.dex.api.http.headers

import akka.http.scaladsl.model.{HttpCharsets, MediaType}

object CustomMediaTypes {
  val `application/hocon` = MediaType.customWithFixedCharset("application", "hocon", HttpCharsets.`UTF-8`)
}
