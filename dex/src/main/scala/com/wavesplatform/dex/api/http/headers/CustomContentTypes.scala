package com.wavesplatform.dex.api.http.headers

import akka.http.scaladsl.model.ContentType

object CustomContentTypes {
  val `application/hocon` = ContentType(CustomMediaTypes.`application/hocon`)
}
