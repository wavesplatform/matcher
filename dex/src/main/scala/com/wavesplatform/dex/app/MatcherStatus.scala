package com.wavesplatform.dex.app

import com.wavesplatform.dex.meta.getSimpleName
import play.api.libs.json.{Format, Reads, Writes}

sealed abstract class MatcherStatus extends Product with Serializable {
  val name: String = getSimpleName(this)
}

object MatcherStatus {
  case object Starting extends MatcherStatus
  case object Working extends MatcherStatus
  case object Stopping extends MatcherStatus

  val All = List(Starting, Stopping, Working)

  implicit val format: Format[MatcherStatus] = Format(
    Reads.StringReads.map { x =>
      All.find(_.name == x) match {
        case Some(r) => r
        case None => throw new IllegalArgumentException(s"Can't parse '$x' as MatcherStatus")
      }
    },
    Writes.StringWrites.contramap(_.name)
  )

}
