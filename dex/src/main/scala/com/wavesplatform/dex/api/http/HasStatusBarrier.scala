package com.wavesplatform.dex.api.http

import akka.http.scaladsl.server.Directive0
import com.wavesplatform.dex.Matcher
import com.wavesplatform.dex.api.http.entities.{DuringShutdown, DuringStart}
import com.wavesplatform.dex.api.routes.ApiRoute

trait HasStatusBarrier { this: ApiRoute =>

  def matcherStatus: () => Matcher.Status

  def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }
}
