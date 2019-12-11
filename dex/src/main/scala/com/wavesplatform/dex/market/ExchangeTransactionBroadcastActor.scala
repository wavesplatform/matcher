package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.market.ExchangeTransactionBroadcastActor._
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.{ScorexLogging, Time}

class ExchangeTransactionBroadcastActor(settings: ExchangeTransactionBroadcastSettings,
                                        time: Time,
                                        check: ExchangeTransaction => Either[ValidationError, Unit],
                                        isConfirmed: ExchangeTransaction => Boolean,
                                        broadcast: Seq[ExchangeTransaction] => Unit)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
    scheduleSend()
  }

  private val default: Receive = {
    case ExchangeTransactionCreated(tx) =>
      try check(tx) match {
        case Right(_) => broadcast(List(tx))
        case Left(e)  => logError(tx, e)
      } catch {
        case e: Throwable => log.error(s"Can't check or broadcast the transaction ${tx.id()}", e)
      }
  }

  private def watching(toCheck: Vector[ExchangeTransaction], next: Vector[ExchangeTransaction]): Receive = {
    case ExchangeTransactionCreated(tx) =>
      try check(tx) match {
        case Right(_) =>
          broadcast(List(tx))
          context.become(watching(toCheck, next :+ tx))

        case Left(e) => logError(tx, e)
      } catch {
        case e: Throwable =>
          log.warn(s"Can't check or broadcast the transaction ${tx.id()}, will try later", e)
          context.become(watching(toCheck, next :+ tx))
      }

    case Send =>
      val nowMs    = time.getTimestamp()
      val expireMs = nowMs - settings.maxPendingTime.toMillis

      val (confirmed, unconfirmed) = toCheck.partition(isConfirmed)
      val (expired, ready)         = unconfirmed.partition(_.timestamp <= expireMs)

      log.debug(s"Stats: ${confirmed.size} confirmed, ${ready.size} ready")
      if (expired.nonEmpty) log.warn(s"${expired.size} expired: ${expired.map(_.id().toString).mkString(", ")}")

      try broadcast(ready)
      catch {
        case e: Throwable => log.warn(s"Can't broadcast one or more of transactions: ${ready.map(_.id()).mkString(", ")}. Will try later", e)
      }

      scheduleSend()
      context.become(watching(next ++ ready, Vector.empty))
  }

  override val receive: Receive = if (settings.broadcastUntilConfirmed) watching(toCheck = Vector.empty, next = Vector.empty) else default

  private def scheduleSend(): Unit = context.system.scheduler.scheduleOnce(settings.interval, self, Send)

  private def logError(tx: ExchangeTransaction, error: ValidationError): Unit = log.warn(s"Tx ${tx.id()} become invalid: $error")
}

object ExchangeTransactionBroadcastActor {
  object Send

  def props(settings: ExchangeTransactionBroadcastSettings,
            time: Time,
            isValid: ExchangeTransaction => Either[ValidationError, Unit],
            isConfirmed: ExchangeTransaction => Boolean,
            broadcast: Seq[ExchangeTransaction] => Unit): Props =
    Props(new ExchangeTransactionBroadcastActor(settings, time, isValid, isConfirmed, broadcast))
}
