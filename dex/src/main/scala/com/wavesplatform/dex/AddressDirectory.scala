package com.wavesplatform.dex

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable

class AddressDirectory(spendableBalanceChanges: Observable[SpendableBalanceChanges],
                       settings: MatcherSettings,
                       addressActorProps: (Address, Boolean) => Props,
                       historyRouter: Option[ActorRef])
    extends Actor
    with ScorexLogging {

  import AddressDirectory._
  import context._

  private var startSchedules: Boolean = false
  private[this] val children          = mutable.AnyRefMap.empty[Address, ActorRef]

  /** Sends balance changes to the AddressActors */
  spendableBalanceChanges.foreach {
    _.foreach { case (address, assetBalances) => children.get(address) foreach (_ ! AddressActor.Event.BalanceUpdated { assetBalances }) }
  } { Scheduler(context.dispatcher) }

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
    case Envelope(address, cmd) => forward(address, cmd)

    case e @ Events.OrderAdded(lo, timestamp) =>
      forward(lo.order.sender, e)
      historyRouter foreach { _ ! SaveOrder(lo, timestamp) }

    case e @ Events.OrderExecuted(submitted, counter, timestamp) =>
      forward(submitted.order.sender, e)
      if (counter.order.sender != submitted.order.sender) forward(counter.order.sender, e)

      lazy val isFirstExecution  = submitted.amount == submitted.order.amount
      lazy val isSubmittedFilled = e.submittedRemainingAmount == 0

      (submitted.isMarket, isFirstExecution, isSubmittedFilled) match {
        case (true, true, _) | (false, true, true) => historyRouter foreach { _ ! SaveOrder(submitted, timestamp) }
        case _                                     => Unit
      }

      historyRouter foreach { _ ! SaveEvent(e) }

    case e @ Events.OrderCanceled(ao, _, timestamp) =>
      forward(ao.order.sender, e)
      if (ao.isMarket && ao.amount == ao.order.amount) historyRouter foreach { _ ! SaveOrder(ao, timestamp) }
      historyRouter foreach { _ ! SaveEvent(e) }

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
  case class Envelope(address: Address, cmd: AddressActor.Message)
  case object StartSchedules
}
