package com.wavesplatform.dex.actors.address

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.utils.{EitherExt2, ScorexLogging}
import com.wavesplatform.dex.history.HistoryRouterActor._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.OrderCancelFailed

import scala.collection.mutable

class AddressDirectoryActor(
  orderDB: OrderDB,
  mkAddressActorProps: (Address, Boolean) => Props,
  historyRouterRef: Option[ActorRef],
  var started: Boolean = false
) extends Actor
    with ScorexLogging {

  import AddressDirectoryActor._
  import context._

  private[this] val children = mutable.AnyRefMap.empty[Address, ActorRef]

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(actorOf(mkAddressActorProps(address, started), address.toString))
  }

  private def forward(address: Address, msg: Any): Unit = (children get address, msg) match {
    case (None, _: AddressActor.Message.BalanceChanged) =>
    case _ => children.getOrElseUpdate(address, createAddressActor(address)) forward msg
  }

  override def receive: Receive = {
    case Envelope(address, cmd) => forward(address, cmd)

    case e @ Events.OrderAdded(lo, _, timestamp) =>
      forward(lo.order.sender, e)
      historyRouterRef foreach { _ ! HistoryInsertMsg.SaveOrder(lo, timestamp) }

    case e: Events.OrderExecuted =>
      Set(e.counter.order, e.submitted.order).map(_.sender).foreach(forward(_, e))
      historyRouterRef foreach { _ ! HistoryInsertMsg.SaveEvent(e) }

    case e: Events.OrderCanceled =>
      forward(e.acceptedOrder.order.sender, e)
      historyRouterRef foreach { _ ! HistoryInsertMsg.SaveEvent(e) }

    case e: OrderCancelFailed =>
      orderDB.get(e.id) match {
        case Some(order) => forward(order.sender.toAddress, e)
        case None => log.warn(s"The order '${e.id}' not found")
      }

    case StartWork =>
      started = true
      context.children.foreach(_ ! StartWork)

    case Terminated(child) =>
      val addressString = child.path.name
      val address = Address.fromString(addressString).explicitGet()
      children.remove(address)
      log.warn(s"Address handler for $addressString terminated")
  }

}

object AddressDirectoryActor {
  val name = "addresses"

  def props(orderDB: OrderDB, mkAddressActorProps: (Address, Boolean) => Props, historyRouterRef: Option[ActorRef]): Props = Props(
    new AddressDirectoryActor(
      orderDB,
      mkAddressActorProps,
      historyRouterRef,
      false
    )
  )

  case class Envelope(address: Address, cmd: AddressActor.Message)
  case object StartWork
}
