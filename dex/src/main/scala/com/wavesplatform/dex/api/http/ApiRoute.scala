package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import com.wavesplatform.dex.Matcher
import com.wavesplatform.dex.api.{DuringShutdown, DuringStart, MatcherResponse, SimpleResponse}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import play.api.libs.json.{Json, OWrites}

trait ApiRoute extends Directives with ApiMarshallers with ScorexLogging {
  protected implicit val matcherResponseTrm: ToResponseMarshaller[MatcherResponse] = MatcherResponse.toResponseMarshaller
//  protected implicit def trm[T: OWrites]: Marshaller[T, MatcherResponse] =
//    Marshaller.opaque[T, MatcherResponse](x => SimpleResponse(StatusCodes.OK, Json.toJsObject(x)))

  def route: Route

  def matcherStatus: () => Matcher.Status

  def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }
}
