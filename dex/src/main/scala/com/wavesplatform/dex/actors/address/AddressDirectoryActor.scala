package com.wavesplatform.dex.actors.address

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.dex.db.OrderDb
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.utils.{EitherExt2, ScorexLogging}
import com.wavesplatform.dex.history.HistoryRouterActor._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.{OrderCancelFailed, OrderCancelFailedFinalized}
import kamon.Kamon

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

class AddressDirectoryActor(
  orderDb: OrderDb[Future],
  mkAddressActorProps: (Address, Boolean) => Props,
  historyRouterRef: Option[ActorRef],
  var recovered: Boolean = false
) extends Actor
    with ScorexLogging {

  import AddressDirectoryActor._
  import context._

  private[this] val children = mutable.AnyRefMap.empty[Address, ActorRef]
  private val emptyOwnerCounter = Kamon.counter("matcher.address-directory-actor.empty-owner").withoutTags()

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(actorOf(mkAddressActorProps(address, recovered), address.toString))
  }

  private def forward(address: Address, msg: Any): Unit = (children get address, msg) match {
    case (None, _: AddressActor.Command.ChangeBalances | _: OrderCancelFailed) =>
    case _ => children.getOrElseUpdate(address, createAddressActor(address)) forward msg
  }

  override def receive: Receive = {
    case Command.ForwardMessage(address, message) =>
      // DEX-1192 docs/places-and-cancels.md
      forward(address, message)

    case command: AddressActor.Command.HasOrderBookEvent =>
      sendEventToHistoryRouter(command)
      command.affectedOrders.map(_.order.sender.toAddress).toSet // Could be one trader
        .foreach(forward(_, command))

    case e: OrderCancelFailed =>
      // We save an order when accept it in AddressActor
      val origSender = sender()
      //TODO remove orderDb in this actor once maybeOwner will be always fulfilled
      e.maybeOwner.map { owner =>
        forward(owner, e)
      }.getOrElse {
        emptyOwnerCounter.increment()
        orderDb.get(e.id).onComplete {
          case Success(Some(order)) =>
            self.tell(OrderCancelFailedFinalized(e, order.sender.toAddress), origSender)
          case Success(None) =>
            log.warn(s"The order '${e.id}' not found")
          case Failure(th) =>
            log.error(s"error while retrieving order by id ${e.id}", th)
        }
      }

    case OrderCancelFailedFinalized(orderCancelFailed, owner) =>
      forward(owner, orderCancelFailed)

    case Terminated(child) =>
      val addressString = child.path.name
      val address = Address.fromString(addressString).explicitGet()
      children.remove(address)
      log.warn(s"Address handler for $addressString terminated")

    case Command.StartWork =>
      recovered = true
      context.children.foreach(_ ! AddressActor.Command.CompleteRecovering)
  }

  private def sendEventToHistoryRouter(command: AddressActor.Command.HasOrderBookEvent): Unit = sendEventToHistoryRouter(command.event)

  private def sendEventToHistoryRouter(event: Events.Event): Unit = historyRouterRef.foreach { historyRouterRef =>
    val msg = event match {
      case Events.OrderAdded(lo, _, timestamp) => HistoryInsertMsg.SaveOrder(lo, timestamp)
      case e: Events.OrderExecuted => HistoryInsertMsg.SaveEvent(e)
      case e: Events.OrderCanceled => HistoryInsertMsg.SaveEvent(e)
    }

    historyRouterRef ! msg
  }

}

object AddressDirectoryActor {
  val name = "addresses"

  def props(orderDB: OrderDb[Future], mkAddressActorProps: (Address, Boolean) => Props, historyRouterRef: Option[ActorRef]): Props = Props(
    new AddressDirectoryActor(
      orderDB,
      mkAddressActorProps,
      historyRouterRef,
      recovered = false
    )
  )

  sealed trait Command extends Product with Serializable

  object Command {
    case class ForwardMessage(address: Address, message: AddressActor.Message)
    case object StartWork
  }

}
