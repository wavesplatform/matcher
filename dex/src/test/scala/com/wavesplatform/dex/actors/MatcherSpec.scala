package com.wavesplatform.dex.actors

import akka.actor.ActorSystem
import akka.testkit.TestKitBase
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.loadConfig
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}

import scala.concurrent.duration.FiniteDuration

abstract class MatcherSpec extends AnyWordSpecLike with MatcherSpecLike

trait MatcherSpecLike extends TestKitBase with Matchers with BeforeAndAfterAll with BeforeAndAfterEach with ScorexLogging with ScalaFutures {
  this: Suite =>

  implicit override lazy val system: ActorSystem = ActorSystem(
    getClass.getSimpleName,
    loadConfig(ConfigFactory.empty())
  )

  implicit override def patienceConfig = PatienceConfig(timeout.duration) // see application.conf
  implicit val timeout = testKitSettings.DefaultTimeout

  override protected def afterAll(): Unit = {
    super.afterAll()
    shutdown(system)
  }

  implicit protected def akkaTimeoutToFiniteDuration(x: Timeout): FiniteDuration = x.duration

}
