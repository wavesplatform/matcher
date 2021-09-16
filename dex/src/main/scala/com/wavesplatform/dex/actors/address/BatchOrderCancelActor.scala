package com.wavesplatform.dex.actors.address

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.wavesplatform.dex.actors.TimedOut
import com.wavesplatform.dex.actors.address.AddressActor.Command.CancelOrder
import com.wavesplatform.dex.actors.address.AddressActor.{Command, Event}
import com.wavesplatform.dex.actors.address.BatchOrderCancelActor.CancelResponse.OrderCancelResult
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error

import scala.collection.immutable.ListMap
import scala.concurrent.duration.FiniteDuration

class BatchOrderCancelActor private (
  initialResponse: ListMap[Order.Id, OrderCancelResult],
  source: Command.Source,
  processorActor: ActorRef,
  clientActor: ActorRef,
  timeout: FiniteDuration
) extends Actor
    with ScorexLogging {

  import BatchOrderCancelActor._
  import context.dispatcher

  initialResponse.foreach {
    case (id, result) => if (result.isRight) processorActor ! CancelOrder(id, source)
  }

  override def receive: Receive = state(initialResponse.keySet, initialResponse, context.system.scheduler.scheduleOnce(timeout, self, TimedOut))

  private def state(restOrderIds: Set[Order.Id], response: Map[Order.Id, OrderCancelResult], timer: Cancellable): Receive = {
    case CancelResponse(id, x) =>
      val updatedRestOrderIds = restOrderIds - id
      val updatedResponse = response.updated(id, x)

      if (updatedRestOrderIds.isEmpty) stop(Event.BatchCancelCompleted(updatedResponse), timer)
      else context.become(state(restOrderIds - id, updatedResponse, timer))

    // case Terminated(ref) => // Can't terminate before processorActor, because processorActor is a parent

    case TimedOut =>
      log.error(s"CancelOrder is timed out for orders: ${restOrderIds.mkString(", ")}")
      stop(Event.BatchCancelCompleted(response), timer)
  }

  private def stop(response: Event.BatchCancelCompleted, timer: Cancellable): Unit = {
    timer.cancel()
    clientActor ! response
    context.stop(self)
  }

}

object BatchOrderCancelActor {

  def props(
    initialResponse: ListMap[Order.Id, OrderCancelResult],
    source: Command.Source,
    processorActor: ActorRef,
    clientActor: ActorRef,
    timeout: FiniteDuration
  ): Props = {
    require(initialResponse.nonEmpty, "orderIds is empty")
    Props(new BatchOrderCancelActor(initialResponse, source, processorActor, clientActor, timeout))
  }

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
