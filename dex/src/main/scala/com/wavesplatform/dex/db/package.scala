package com.wavesplatform.dex

import kamon.Kamon
import kamon.tag.TagSet

import scala.util.chaining._

package object db {

  private val timer = Kamon.timer("matcher.leveldb")

  def measureDb[T](cls: String, method: String)(f: () => T): T =
    mkTimer(cls, method)
      .start()
      .pipe { startedTimer =>
        f().tap(_ => startedTimer.stop())
      }

  private def mkTimer(cls: String, method: String) =
    timer.withTags(TagSet.from(Map(
      "cls" -> cls,
      "method" -> method
    )))

}
