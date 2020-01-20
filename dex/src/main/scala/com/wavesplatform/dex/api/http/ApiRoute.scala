package com.wavesplatform.dex.api.http

import akka.http.scaladsl.server.{Directives, Route}
import com.wavesplatform.dex.domain.utils.ScorexLogging

trait ApiRoute extends Directives with ApiMarshallers with ScorexLogging {
  def route: Route
}
