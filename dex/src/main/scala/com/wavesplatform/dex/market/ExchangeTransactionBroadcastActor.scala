package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.market.ExchangeTransactionBroadcastActor._
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

class ExchangeTransactionBroadcastActor(settings: ExchangeTransactionBroadcastSettings,
                                        time: Time,
                                        isConfirmed: ByteStr => Boolean,
                                        broadcast: Seq[ExchangeTransaction] => Set[ByteStr])
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
    scheduleSend()
  }

  private val default: Receive = {
    case ExchangeTransactionCreated(tx) => broadcast(List(tx))
  }

  private def watching(toCheck: Vector[ExchangeTransaction], next: Vector[ExchangeTransaction]): Receive = {
    case ExchangeTransactionCreated(tx) =>
      if (broadcast(List(tx)).nonEmpty) context.become(watching(toCheck, next :+ tx))

    case Send =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      val (confirmed, unconfirmed) = toCheck.partition(tx => isConfirmed(tx.id()))
      val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

      val (validTxs, invalidTxs) = {
        val validTxIds = broadcast(ready)
        ready.partition(tx => validTxIds.contains(tx.id()))
      }

      log.debug(s"Stats: ${confirmed.size} confirmed, ${ready.size} sent, ${validTxs.size} successful")
      if (expired.nonEmpty) log.warn(s"${expired.size} failed to send: ${format(expired)}; became invalid: ${format(invalidTxs)}")

      scheduleSend()
      context.become(watching(next ++ validTxs, Vector.empty))
  }

  override val receive: Receive = if (settings.broadcastUntilConfirmed) watching(toCheck = Vector.empty, next = Vector.empty) else default

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, Send)

  private def format(txs: Iterable[ExchangeTransaction]): String = txs.map(_.id().toString).mkString(", ")
}

object ExchangeTransactionBroadcastActor {
  object Send

  def props(settings: ExchangeTransactionBroadcastSettings,
            time: Time,
            isConfirmed: ByteStr => Boolean,
            broadcast: Seq[ExchangeTransaction] => Set[ByteStr]): Props =
    Props(new ExchangeTransactionBroadcastActor(settings, time, isConfirmed, broadcast))
}
