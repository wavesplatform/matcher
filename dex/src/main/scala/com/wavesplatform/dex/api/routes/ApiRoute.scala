package com.wavesplatform.dex.api.routes

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.{Directives, Route}
import com.wavesplatform.dex.api.http.ApiMarshallers
import com.wavesplatform.dex.api.http.entities.MatcherResponse
import com.wavesplatform.dex.domain.utils.ScorexLogging

trait ApiRoute extends Directives with ApiMarshallers with ScorexLogging {

  protected implicit val matcherResponseTrm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller

  def route: Route
}
