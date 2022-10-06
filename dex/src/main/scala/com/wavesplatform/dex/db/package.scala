package com.wavesplatform.dex

import kamon.Kamon
import kamon.tag.TagSet

package object db {

  private val timer = Kamon.timer("matcher.leveldb")

  def measureDb[F[_]: OnComplete, T](cls: String, method: String)(block: => F[T]): F[T] = {
    val startedTimer = mkTimer(cls, method).start()
    val startedBlock = block
    OnComplete[F].onComplete(startedBlock)(_ => startedTimer.stop())
    startedBlock
  }

  private def mkTimer(cls: String, method: String) =
    timer.withTags(TagSet.from(Map(
      "cls" -> cls,
      "method" -> method
    )))

}
