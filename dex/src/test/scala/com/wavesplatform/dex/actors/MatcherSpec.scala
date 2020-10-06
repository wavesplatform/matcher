package com.wavesplatform.dex.actors

import akka.actor.ActorSystem
import akka.testkit.TestKitBase
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.settings.loadConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}

abstract class MatcherSpec(_actorSystemName: String) extends AnyWordSpecLike with MatcherSpecLike {
  protected def actorSystemName: String = _actorSystemName
}

trait MatcherSpecLike extends TestKitBase with Matchers with BeforeAndAfterAll with BeforeAndAfterEach with ScorexLogging {
  this: Suite =>

  protected def actorSystemName: String

  implicit override lazy val system: ActorSystem = ActorSystem(
    actorSystemName,
    loadConfig(ConfigFactory.empty())
  )

  override protected def afterAll(): Unit = {
    super.afterAll()
    shutdown(system)
  }

}
