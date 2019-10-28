package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.market.ExchangeTransactionBroadcastActor._
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

import scala.concurrent.Future

class ExchangeTransactionBroadcastActor(settings: ExchangeTransactionBroadcastSettings,
                                        time: Time,
                                        isConfirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
                                        broadcast: ExchangeTransaction => Future[Boolean])
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
    scheduleSend()
  }

  private val default: Receive = { case ExchangeTransactionCreated(tx) => broadcast(tx) }

  private def watching(toCheck: Vector[ExchangeTransaction], broadcasted: Vector[ExchangeTransaction]): Receive = {
    case Send =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      isConfirmed { toCheck map (_.id()) }.flatMap { isTxConfirmed =>
        val (confirmed, unconfirmed) = toCheck.partition(tx => isTxConfirmed(tx.id.value))
        val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

        Future.sequence { ready map (tx => broadcast(tx) map (tx -> _)) } map (_.toMap) map { isTxBroadcasted =>
          val (validTxs, invalidTxs) = ready.partition(isTxBroadcasted)

          log.debug(s"Stats: ${confirmed.size} confirmed, ${ready.size} sent, ${validTxs.size} successful")
          if (expired.nonEmpty) log.warn(s"${expired.size} failed to send: ${format(expired)}; became invalid: ${format(invalidTxs)}")

          validTxs
        }
      } foreach (broadcastedTxs => self ! CheckBroadcastedTransactions(broadcastedTxs))

    case ExchangeTransactionCreated(tx)    => broadcast(tx) foreach { if (_) self ! TransactionIsBroadcasted(tx) }
    case TransactionIsBroadcasted(tx)      => context.become { watching(toCheck, broadcasted :+ tx) }
    case CheckBroadcastedTransactions(txs) => scheduleSend(); context.become { watching(broadcasted ++ txs, Vector.empty) }
  }

  override val receive: Receive = if (settings.broadcastUntilConfirmed) watching(toCheck = Vector.empty, broadcasted = Vector.empty) else default

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, Send)

  private def format(txs: Iterable[ExchangeTransaction]): String = txs.map(_.id().toString).mkString(", ")
}

object ExchangeTransactionBroadcastActor {

  final case object Send
  final case class TransactionIsBroadcasted(tx: ExchangeTransaction)
  final case class CheckBroadcastedTransactions(txs: Seq[ExchangeTransaction])

  def props(settings: ExchangeTransactionBroadcastSettings,
            time: Time,
            isConfirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
            broadcast: ExchangeTransaction => Future[Boolean]): Props =
    Props(new ExchangeTransactionBroadcastActor(settings, time, isConfirmed, broadcast))
}
