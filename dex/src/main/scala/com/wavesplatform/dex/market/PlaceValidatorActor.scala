package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef}
import com.wavesplatform.dex.NewAddressActor
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.market.PlaceValidatorActor.Event
import com.wavesplatform.dex.model._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.immutable.Queue

class PlaceValidatorActor(ownerActor: ActorRef, balancesActor: ActorRef, orderBookCache: AssetPair => OrderBook.AggregatedSnapshot) extends Actor {
  import NewAddressActor._

  override def receive: Receive = processing(0, Queue.empty)

  private def processing(currentRequestId: Long, queue: Queue[PlaceOrder]): Receive = {
    case command: PlaceOrder =>
      import command.order
      val updatedQueue = queue.enqueue(command)
      val updatedRequestId = if (queue.isEmpty) {
        val r = currentRequestId + 1
        balancesActor ! BalanceActor.Query.GetTradableBalance(r, order.sender, Set(order.getSpendAssetId, order.matcherFeeAssetId))
        r
      } else currentRequestId

      context.become(processing(updatedRequestId, updatedQueue))

    case BalanceActor.Reply.TradableBalance(requestId, _, tradableBalance) if requestId == currentRequestId =>
      queue.dequeueOption.foreach {
        case (currentCommand, rest) =>
          val acceptedOrder = currentCommand.tpe match {
            case OrderType.Limit  => MarketOrder(currentCommand.order, tradableBalance)
            case OrderType.Market => LimitOrder(currentCommand.order)
          }

          val orderId = currentCommand.order.id()
          val validationResult = accountStateValidator(acceptedOrder, tradableBalance) match {
            case Left(error) => Event.ValidationFailed(orderId, error)
            case Right(_)    => Event.ValidationPassed(orderId)
          }
          ownerActor ! validationResult

          rest.dequeueOption.foreach {
            case (nextCommand, _) =>
              import nextCommand.order
              val updatedRequestId = currentRequestId + 1
              balancesActor ! BalanceActor.Query.GetTradableBalance(updatedRequestId,
                                                                    order.sender,
                                                                    Set(order.getSpendAssetId, order.matcherFeeAssetId))
              context.become(processing(updatedRequestId, rest))
          }
      }
  }

  private def accountStateValidator(acceptedOrder: AcceptedOrder, tradableBalance: Asset => Long): OrderValidator.Result[AcceptedOrder] =
    OrderValidator.accountStateAware(
      acceptedOrder.order.sender,
      tradableBalance,
      ???, // activeOrders,
      ???, //id => activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrder(id),
      orderBookCache
    )(acceptedOrder)
}

object PlaceValidatorActor {
  sealed trait Event {
    def orderId: Order.Id
  }

  object Event {
    case class ValidationFailed(orderId: Order.Id, error: MatcherError) extends Event
    case class ValidationPassed(orderId: Order.Id)                      extends Event
  }
}
