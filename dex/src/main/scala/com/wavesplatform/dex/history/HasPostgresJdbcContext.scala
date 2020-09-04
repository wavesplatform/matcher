package com.wavesplatform.dex.history

import com.typesafe.config.Config
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.EventReason
import io.getquill.{MappedEncoding, PostgresJdbcContext, SnakeCase}

import scala.annotation.unused

trait HasPostgresJdbcContext {

  def connectionConfig: Config

  protected lazy val ctx: PostgresJdbcContext[SnakeCase.type] = new PostgresJdbcContext(SnakeCase, connectionConfig)

  @unused
  protected implicit val encodeEventReason = MappedEncoding[EventReason, Byte] {
    case Events.NotTracked                              => 0
    case Events.OrderAddedReason.RequestExecuted        => 1
    case Events.OrderAddedReason.OrderBookRecovered     => 2
    case Events.OrderExecutedReason                     => 3
    case Events.OrderCanceledReason.RequestExecuted     => 4
    case Events.OrderCanceledReason.OrderBookDeleted    => 5
    case Events.OrderCanceledReason.Expired             => 6
    case Events.OrderCanceledReason.BecameUnmatchable   => 7
    case Events.OrderCanceledReason.BecameInvalid       => 8
    case Events.OrderCanceledReason.InsufficientBalance => 9
  }

  @unused
  protected implicit val decodeEventReason = MappedEncoding[Byte, EventReason] {
    case 0 => Events.NotTracked
    case 1 => Events.OrderAddedReason.RequestExecuted
    case 2 => Events.OrderAddedReason.OrderBookRecovered
    case 3 => Events.OrderExecutedReason
    case 4 => Events.OrderCanceledReason.RequestExecuted
    case 5 => Events.OrderCanceledReason.OrderBookDeleted
    case 6 => Events.OrderCanceledReason.Expired
    case 7 => Events.OrderCanceledReason.BecameUnmatchable
    case 8 => Events.OrderCanceledReason.BecameInvalid
    case 9 => Events.OrderCanceledReason.InsufficientBalance
  }
}
