package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.dex.AddressActor.Command.CancelOrder
import com.wavesplatform.dex.api.MatcherResponse
import com.wavesplatform.dex.{api, error}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.Order.Id

class MultipleOrderCancelActor(orderIds: Set[Order.Id], processorActor: ActorRef, clientActor: ActorRef) extends Actor {
  import MultipleOrderCancelActor._
  orderIds.foreach(processorActor ! CancelOrder(_))

  override def receive: Receive = state(orderIds, Map.empty)

  private def state(restOrderIds: Set[Order.Id], response: Map[Order.Id, api.MatcherResponse]): Receive = {
    case CancelResponse(id, x) =>
      val updatedRestOrderIds = restOrderIds - id
      val updatedResponse     = response.updated(id, x)

      if (updatedRestOrderIds.isEmpty) {
        clientActor ! api.BatchCancelCompleted(response)
        context.stop(self)
      } else context.become(state(restOrderIds - id, updatedResponse))
  }
}

object MultipleOrderCancelActor {
  def props(orderIds: Set[Order.Id], processorActor: ActorRef, clientActor: ActorRef): Props =
    Props(new MultipleOrderCancelActor(orderIds, processorActor, clientActor))

  object CancelResponse {
    def unapply(arg: Any): Option[(Id, MatcherResponse)] = helper.lift(arg)

    private val helper: PartialFunction[Any, (Order.Id, api.MatcherResponse)] = {
      case x @ api.OrderCanceled(id)                                => (id, x)
      case x @ api.OrderCancelRejected(error.OrderNotFound(id))     => (id, x)
      case x @ api.OrderCancelRejected(error.OrderCanceled(id))     => (id, x)
      case x @ api.OrderCancelRejected(error.OrderFull(id))         => (id, x)
      case x @ api.OrderCancelRejected(error.MarketOrderCancel(id)) => (id, x)
    }
  }
}
