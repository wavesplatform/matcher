package com.wavesplatform.dex.actors.address

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.wavesplatform.dex.actors.TimedOut
import com.wavesplatform.dex.actors.address.AddressActor.Command.CancelOrder
import com.wavesplatform.dex.actors.address.AddressActor.{Command, Event}
import com.wavesplatform.dex.actors.address.BatchOrderCancelActor.CancelResponse.OrderCancelResult
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.RequestTimeout

import scala.collection.immutable.ListMap
import scala.concurrent.duration.FiniteDuration

class BatchOrderCancelActor private (
  orderIds: List[Order.Id],
  source: Command.Source,
  processorActor: ActorRef,
  clientActor: ActorRef,
  timeout: FiniteDuration
) extends Actor
    with ScorexLogging {

  import BatchOrderCancelActor._
  import context.dispatcher

  private val uniqueOrderIds: Set[Order.Id] = orderIds.toSet
  uniqueOrderIds.foreach(id => processorActor ! CancelOrder(id, source))

  override def receive: Receive = state(uniqueOrderIds, Map.empty, context.system.scheduler.scheduleOnce(timeout, self, TimedOut))

  private def state(restOrderIds: Set[Order.Id], response: Map[Order.Id, OrderCancelResult], timer: Cancellable): Receive = {

    case CancelResponse(id, x) =>
      val updatedRestOrderIds = restOrderIds - id
      val updatedResponse = response.updated(id, x)

      if (updatedRestOrderIds.isEmpty)
        stop(Event.BatchCancelCompleted(mkOrderedResponse(updatedResponse)), timer)
      else context.become(state(restOrderIds - id, updatedResponse, timer))

    // case Terminated(ref) => // Can't terminate before processorActor, because processorActor is a parent

    case TimedOut =>
      log.error(s"CancelOrder is timed out for orders: ${restOrderIds.mkString(", ")}")
      stop(Event.BatchCancelCompleted(mkOrderedResponse(response)), timer)
  }

  private def mkOrderedResponse(response: Map[Order.Id, OrderCancelResult]): ListMap[Order.Id, OrderCancelResult] =
    orderIds
      .foldLeft(ListMap.newBuilder[Order.Id, OrderCancelResult]) { (acc, id) =>
        val result = response.getOrElse(id, Left(RequestTimeout))
        acc.addOne((id, result))
      }.result()

  private def stop(response: Event.BatchCancelCompleted, timer: Cancellable): Unit = {
    timer.cancel()
    clientActor ! response
    context.stop(self)
  }

}

object BatchOrderCancelActor {

  def props(
    orderIds: List[Order.Id],
    source: Command.Source,
    processorActor: ActorRef,
    clientActor: ActorRef,
    timeout: FiniteDuration
  ): Props =
    Props(new BatchOrderCancelActor(orderIds, source, processorActor, clientActor, timeout))

  object CancelResponse {

    type OrderCancelResult = Either[error.MatcherError, Event.OrderCanceled]

    def unapply(arg: Any): Option[(Order.Id, OrderCancelResult)] = helper.lift(arg)

    private val helper: PartialFunction[Any, (Order.Id, OrderCancelResult)] = {
      case x @ Event.OrderCanceled(id) => (id, Right(x))
      case x @ error.OrderNotFound(id) => (id, Left(x))
      case x @ error.OrderCanceled(id) => (id, Left(x))
      case x @ error.OrderFull(id) => (id, Left(x))
      case x @ error.MarketOrderCancel(id) => (id, Left(x))
    }

  }

}
