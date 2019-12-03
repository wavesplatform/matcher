package com.wavesplatform.dex.market

import akka.actor.{Actor, Props}
import com.wavesplatform.account.Address
import com.wavesplatform.dex.market.CreateExchangeTransactionActor.OrderExecutedObserved
import com.wavesplatform.dex.model.Events.{ExchangeTransactionCreated, OrderExecuted}
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.Json

import scala.collection.mutable

class CreateExchangeTransactionActor(createTransaction: CreateTransaction) extends Actor with ScorexLogging {
  private val pendingEvents = mutable.Set.empty[OrderExecuted]

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[OrderExecutedObserved])

  override def receive: Receive = {
    case OrderExecutedObserved(sender, event) =>
      val sameOwner = event.counter.order.sender == event.submitted.order.sender
      log.debug(
        s"Execution observed at $sender for OrderExecuted(${event.submitted.order.id()}, ${event.counter.order.id()}), amount=${event.executedAmount})${if (sameOwner) " Same owner for both orders" else ""}")
      if (sameOwner || pendingEvents.contains(event)) {
        import event.{counter, submitted}
        createTransaction(event) match {
          case Right(tx) =>
            log.info(s"Created transaction: $tx")
            context.system.eventStream.publish(ExchangeTransactionCreated(tx))
          case Left(ex) =>
            log.warn(
              s"""Can't create tx: $ex
               |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
               |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
            )
        }

        pendingEvents -= event
      } else pendingEvents += event
  }
}

object CreateExchangeTransactionActor {
  val name = "create-exchange-tx"

  case class OrderExecutedObserved(sender: Address, event: OrderExecuted)

  def props(createTransaction: CreateTransaction): Props = Props(new CreateExchangeTransactionActor(createTransaction))
}
