package com.wavesplatform.dex.actors.tx

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.db.ExchangeTxStorage
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.Events._

import scala.concurrent.Future
import scala.util.Failure

class WriteExchangeTransactionActor(storage: ExchangeTxStorage[Future]) extends Actor with ScorexLogging {

  import context.dispatcher

  // TODO DEX-1081 Batching
  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      log.trace(s"Appending ${tx.id()} to orders [${tx.buyOrder.idStr()}, ${tx.sellOrder.idStr()}]")
      storage.put(tx).onComplete {
        case Failure(e) => log.warn(s"Can't write ${tx.id}", e)
        case _ =>
      }
  }

}

object WriteExchangeTransactionActor {
  val name: String = "WriteExchangeTransactionActor"
  def props(storage: ExchangeTxStorage[Future]): Props = Props(new WriteExchangeTransactionActor(storage))
}
