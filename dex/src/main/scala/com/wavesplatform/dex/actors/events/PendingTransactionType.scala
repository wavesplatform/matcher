package com.wavesplatform.dex.actors.events

sealed trait PendingTransactionType

object PendingTransactionType {
  case object KnownOnMatcher extends PendingTransactionType

  // Will be known soon on Matcher. Another case is impossible, because we check a transaction first
  case object KnownOnNode extends PendingTransactionType
}
