package com.wavesplatform.dex

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.utils.{EitherExt2, ScorexLogging}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.OrderCancelFailed
import com.wavesplatform.dex.settings.MatcherSettings
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable
import scala.util.{Failure, Success}

class AddressDirectory(spendableBalanceChanges: Observable[SpendableBalanceChanges],
                       settings: MatcherSettings,
                       orderDB: OrderDB,
                       addressActorProps: (Address, Boolean) => Props,
                       historyRouter: Option[ActorRef])
    extends Actor
    with ScorexLogging {

  import AddressDirectory._
  import context._

  private var startSchedules: Boolean = false
  private[this] val children          = mutable.AnyRefMap.empty[Address, ActorRef]

  /** Sends balance changes to the AddressActors */
  spendableBalanceChanges
    .foreach(x => self ! BalanceChanged(x))(Scheduler(context.dispatcher))
    .onComplete {
      case Success(_) => log.info("Spendable balance changes stream stopped")
      case Failure(e) => log.error("Found an error in spendable balance changes stream. It was stopped", e)
    }

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def createAddressActor(address: Address): ActorRef = {
    log.debug(s"Creating address actor for $address")
    watch(actorOf(addressActorProps(address, startSchedules), address.toString))
  }

  private def forward(address: Address, msg: Any): Unit = {
    val handler = children.getOrElseUpdate(address, createAddressActor(address))
    handler.forward(msg)
  }

  override def receive: Receive = {
    case msg: BalanceChanged =>
      msg.xs.foreach {
        case (address, assetBalances) => children.get(address) foreach (_ ! AddressActor.Command.CancelNotEnoughCoinsOrders { assetBalances })
      }

    case Envelope(address, cmd) => forward(address, cmd)

    case e @ Events.OrderAdded(lo, timestamp) =>
      forward(lo.order.sender, e)
      historyRouter foreach { _ ! SaveOrder(lo, timestamp) }

    case e @ Events.OrderExecuted(submitted, counter, timestamp, _, _) =>
      forward(submitted.order.sender, e)
      if (counter.order.sender != submitted.order.sender) forward(counter.order.sender, e)

      lazy val isFirstExecution = submitted.amount == submitted.order.amount
      lazy val isSubmittedFilled = !e.submittedRemaining.isValid

      (submitted.isMarket, isFirstExecution, isSubmittedFilled) match {
        case (true, true, _) | (false, true, true) => historyRouter foreach { _ ! SaveOrder(submitted, timestamp) }
        case _                                     => Unit
      }

      historyRouter foreach { _ ! SaveEvent(e) }

    case e @ Events.OrderCanceled(ao, _, timestamp) =>
      forward(ao.order.sender, e)
      if (ao.isMarket && ao.amount == ao.order.amount) historyRouter foreach { _ ! SaveOrder(ao, timestamp) }
      historyRouter foreach { _ ! SaveEvent(e) }

    case e: OrderCancelFailed =>
      orderDB.get(e.id) match {
        case Some(order) => forward(order.sender.toAddress, e)
        case None        => log.warn(s"The order '${e.id}' not found")
      }

    case StartSchedules =>
      if (!startSchedules) {
        startSchedules = true
        context.children.foreach(_ ! StartSchedules)
      }

    case Terminated(child) =>
      val addressString = child.path.name
      val address       = Address.fromString(addressString).explicitGet()
      children.remove(address)
      log.warn(s"Address handler for $addressString terminated")
  }
}

object AddressDirectory {
  case class BalanceChanged(xs: SpendableBalanceChanges) extends AnyVal
  case class Envelope(address: Address, cmd: AddressActor.Message)
  case object StartSchedules
}
