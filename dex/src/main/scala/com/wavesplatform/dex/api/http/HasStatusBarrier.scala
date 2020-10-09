package com.wavesplatform.dex.api.http

import akka.http.scaladsl.server.Directive0
import com.wavesplatform.dex.api.http.entities.{DuringShutdown, DuringStart}
import com.wavesplatform.dex.api.routes.ApiRoute
import com.wavesplatform.dex.app.MatcherStatus

trait HasStatusBarrier { this: ApiRoute =>

  def matcherStatus: () => MatcherStatus

  def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case MatcherStatus.Working => pass
    case MatcherStatus.Starting => complete(DuringStart)
    case MatcherStatus.Stopping => complete(DuringShutdown)
  }

}
