package com.wavesplatform.dex.grpc.integration.utx

import cats.syntax.option._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

import scala.collection.immutable.Queue

// todo: docs/comment
case class UtxQueue(
  queue: Queue[ExchangeTransaction] = Queue.empty,
  pending: Option[ExchangeTransaction] = None
) {

  def onTxAdded(tx: Transaction): (Option[ExchangeTransaction], UtxQueue) =
    if (pending.exists(_.id() == tx.id())) deqeue()
    else (none, this)

  def enqueue(tx: ExchangeTransaction): (Option[ExchangeTransaction], UtxQueue) =
    if (pending.isEmpty)
      (Some(tx), copy(pending = tx.some))
    else if (!queue.contains(tx))
      (none, copy(queue = queue.enqueue(tx)))
    else
      (none, this)

  def deqeue(): (Option[ExchangeTransaction], UtxQueue) =
    queue.dequeueOption.map { case (t, q) =>
      (Some(t), copy(queue = q, pending = t.some))
    }.getOrElse((None, copy(pending = none)))

}
