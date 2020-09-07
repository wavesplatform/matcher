package com.wavesplatform.dex.actors.tx

import akka.actor.{Actor, Props}
import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.functor._
import com.wavesplatform.dex.actors.tx.BroadcastExchangeTransactionActor._
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.dex.time.Time

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class BroadcastExchangeTransactionActor(settings: ExchangeTransactionBroadcastSettings,
                                        time: Time,
                                        confirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
                                        broadcast: ExchangeTransaction => Future[Boolean])
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  private val default: Receive = {
    case ExchangeTransactionCreated(tx) => broadcast(tx)
    case CheckAndSend                   => // ignore
  }

  private def watching(toCheck: Vector[ExchangeTransaction], toNextCheck: Vector[ExchangeTransaction]): Receive = {
    case CheckAndSend =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      confirmed { toCheck.map(_.id()) }
        .flatMap { confirmations =>
          val (confirmed, unconfirmed) = toCheck.partition(tx => confirmations(tx.id.value()))
          val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

          Future
            .sequence { ready.map(tx => broadcast(tx).tupleLeft(tx)) }
            .map(_.toMap)
            .map { isTxBroadcasted =>
              val (validTxs, invalidTxs) = ready.partition(isTxBroadcasted)

              log.debug(s"Stats: ${confirmed.size} confirmed, ${ready.size} sent, ${validTxs.size} successful")
              if (expired.nonEmpty) log.warn(s"${expired.size} failed to send: ${format(expired)}; became invalid: ${format(invalidTxs)}")

              validTxs
            }
        }
        .recover {
          case NonFatal(e) =>
            log.warn(s"Can't process transactions", e)
            toCheck
        }
        .foreach { broadcastedTxs =>
          self ! StashTransactionsToCheck(broadcastedTxs)
        }

    case ExchangeTransactionCreated(tx) =>
      val r = for {
        confirmed <- confirmed(List(tx.id())).map(_.getOrElse(tx.id(), false))
        _         <- if (confirmed) Future.unit else broadcast(tx)
      } yield confirmed

      r.onComplete {
        case Success(confirmed) => if (!confirmed) self ! EnqueueToNextCheck(tx)
        case Failure(e) =>
          log.warn(s"Can't confirm or broadcast ${tx.id()}", e)
          self ! EnqueueToCheck(tx)
      }

    case EnqueueToCheck(tx)            => context.become { watching(toCheck :+ tx, toNextCheck) }
    case EnqueueToNextCheck(tx)        => context.become { watching(toCheck, toNextCheck :+ tx) }
    case StashTransactionsToCheck(txs) => scheduleSend(); context.become { watching(toNextCheck ++ txs, Vector.empty) }
  }

  override val receive: Receive = if (settings.broadcastUntilConfirmed) {
    scheduleSend()
    watching(toCheck = Vector.empty, toNextCheck = Vector.empty)
  } else default

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, CheckAndSend)

  private def format(txs: Iterable[ExchangeTransaction]): String = txs.map(_.id().toString).mkString(", ")
}

object BroadcastExchangeTransactionActor {

  final case object CheckAndSend

  private final case class EnqueueToNextCheck(tx: ExchangeTransaction)
  private final case class EnqueueToCheck(tx: ExchangeTransaction)
  private final case class StashTransactionsToCheck(txs: Seq[ExchangeTransaction])

  def props(settings: ExchangeTransactionBroadcastSettings,
            time: Time,
            isConfirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
            broadcast: ExchangeTransaction => Future[Boolean]): Props =
    Props(new BroadcastExchangeTransactionActor(settings, time, isConfirmed, broadcast))
}
