package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import com.wavesplatform.dex.Matcher
import com.wavesplatform.dex.api.{DuringShutdown, DuringStart, MatcherResponse}
import com.wavesplatform.dex.domain.utils.ScorexLogging

trait ApiRoute extends Directives with ApiMarshallers with ScorexLogging {
  protected implicit val matcherResponseTrm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller

  def route: Route

  def matcherStatus: () => Matcher.Status

  def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }
}
