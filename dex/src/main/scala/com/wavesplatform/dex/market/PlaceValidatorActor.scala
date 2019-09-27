package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef}
import com.wavesplatform.dex.AddressActor
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.market.PlaceValidatorActor.{Event, _}
import com.wavesplatform.dex.model._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.Order.Id
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.immutable.Queue

class PlaceValidatorActor(ownerActor: ActorRef, balanceActor: ActorRef, orderBookCache: AssetPair => OrderBook.AggregatedSnapshot) extends Actor {
  override def receive: Receive = processing(-1, Queue.empty)

  private def processing(currentRequestId: Long, queue: Queue[Command]): Receive = {
    case command: Command.Validate => processNext(queue.enqueue(command))

    case BalanceActor.Reply.TradableBalance(requestId, tradableBalance) if requestId == currentRequestId =>
      queue.dequeueOption.foreach {
        case (currentCommand: Command.Validate, restQueue) if requestId == currentCommand.requestId =>
          val acceptedOrder = currentCommand.place.toAcceptedOrder(tradableBalance)
          val validationResult = accountStateValidator(acceptedOrder, tradableBalance) match {
            case Left(error) => Event.ValidationFailed(acceptedOrder.order.id(), error)
            case Right(_)    => Event.ValidationPassed(acceptedOrder)
          }
          ownerActor ! validationResult
          processNext(restQueue)
      }
  }

  private def processNext(queue: Queue[Command]): Unit = queue.dequeueOption.foreach {
    case (nextCommand: Command.Validate, _) =>
      import nextCommand.place.order
      import nextCommand.requestId
      balanceActor ! BalanceActor.Query.GetTradableBalance(requestId, order.sender, Set(order.getSpendAssetId, order.matcherFeeAssetId))
      context.become(processing(requestId, queue))
  }

  private def accountStateValidator(acceptedOrder: AcceptedOrder, tradableBalance: Asset => Long): OrderValidator.Result[AcceptedOrder] =
    OrderValidator.accountStateAware(acceptedOrder.order.sender, tradableBalance, orderBookCache)(acceptedOrder)
}

object PlaceValidatorActor {
  sealed trait Command
  object Command {
    case class Validate(requestId: Long, place: AddressActor.Command.PlaceOrder) extends Command
  }

  sealed trait Event {
    def orderId: Order.Id
  }

  object Event {
    case class ValidationFailed(orderId: Order.Id, error: MatcherError) extends Event
    case class ValidationPassed(acceptedOrder: AcceptedOrder) extends Event {
      override def orderId: Id = acceptedOrder.order.id()
    }
  }
}
