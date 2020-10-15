package com.wavesplatform.dex.it.api

import com.softwaremill.sttp.Response
import play.api.libs.json.Reads

case class EnrichedResponse[T](response: Response[String])(implicit val reads: Reads[T])
