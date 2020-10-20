package com.wavesplatform.dex.app

sealed trait MatcherStatus extends Product with Serializable

object MatcherStatus {
  case object Starting extends MatcherStatus
  case object Working extends MatcherStatus
  case object Stopping extends MatcherStatus
}
