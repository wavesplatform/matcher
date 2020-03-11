package com.wavesplatform.dex.api

import akka.http.scaladsl.testkit._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

abstract class RouteSpec(basePath: String) extends AnyFreeSpec with ScalatestRouteTest with Matchers {
  protected def routePath(suffix: String) = s"$basePath$suffix"
}
