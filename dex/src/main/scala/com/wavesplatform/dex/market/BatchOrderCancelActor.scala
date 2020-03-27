package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.wavesplatform.dex.AddressActor.Command.CancelOrder
import com.wavesplatform.dex.actors.TimedOut
import com.wavesplatform.dex.api.MatcherResponse
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.Order.Id
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.{api, error}

import scala.concurrent.duration.FiniteDuration

class BatchOrderCancelActor private (
    orderIds: Set[Order.Id],
    processorActor: ActorRef,
    clientActor: ActorRef,
    timeout: FiniteDuration,
    initResponse: Map[Order.Id, api.MatcherResponse]
) extends Actor
    with ScorexLogging {

  import BatchOrderCancelActor._
  import context.dispatcher

  orderIds.foreach(processorActor ! CancelOrder(_))

  override def receive: Receive = state(orderIds, initResponse, context.system.scheduler.scheduleOnce(timeout, self, TimedOut))

  private def state(restOrderIds: Set[Order.Id], response: Map[Order.Id, api.MatcherResponse], timer: Cancellable): Receive = {
    case CancelResponse(id, x) =>
      val updatedRestOrderIds = restOrderIds - id
      val updatedResponse     = response.updated(id, x)

      if (updatedRestOrderIds.isEmpty) stop(api.BatchCancelCompleted(updatedResponse), timer)
      else context.become(state(restOrderIds - id, updatedResponse, timer))

    // case Terminated(ref) => // Can't terminate before processorActor, because processorActor is a parent

    case TimedOut =>
      log.error(s"CancelOrder is timed out for orders: ${restOrderIds.mkString(", ")}")
      stop(api.BatchCancelCompleted(response), timer)
  }

  private def stop(response: api.BatchCancelCompleted, timer: Cancellable): Unit = {
    timer.cancel()
    clientActor ! response
    context.stop(self)
  }
}

object BatchOrderCancelActor {
  def props(orderIds: Set[Order.Id],
            processorActor: ActorRef,
            clientActor: ActorRef,
            timeout: FiniteDuration,
            initResponse: Map[Order.Id, api.MatcherResponse] = Map.empty): Props = {
    require(orderIds.nonEmpty, "orderIds is empty")
    Props(new BatchOrderCancelActor(orderIds, processorActor, clientActor, timeout, initResponse))
  }

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
