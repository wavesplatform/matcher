package com.wavesplatform.dex

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.collection.mutable

class AddressDirectory(spendableBalanceChanged: Observable[(Address, Asset)],
                       settings: MatcherSettings,
                       addressActorProps: (Address, Boolean) => Props,
                       historyRouter: Option[ActorRef])
    extends Actor
    with ScorexLogging {

  import AddressDirectory._
  import context._

  private var startSchedules: Boolean = false
  private[this] val children          = mutable.AnyRefMap.empty[Address, ActorRef]

  spendableBalanceChanged
    .filter(x => children.contains(x._1))
    .bufferTimed(settings.balanceWatchingBufferInterval)
    .filter(_.nonEmpty)
    .foreach { changes =>
      val acc = mutable.Map.empty[Address, Set[Asset]]

      changes.foreach { case (addr, changed)   => acc.update(addr, acc.getOrElse(addr, Set.empty) + changed) }
      acc.foreach { case (addr, changedAssets) => children.get(addr).foreach(_ ! AddressActor.BalanceUpdated(changedAssets)) }

    }(Scheduler(context.dispatcher))

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
        case (true, true, _)     => historyRouter foreach { _ ! SaveOrder(submitted, timestamp) }
        case (false, true, true) => historyRouter foreach { _ ! SaveOrder(submitted, timestamp) }
        case _                   => Unit
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
  case class Envelope(address: Address, cmd: AddressActor.Command)
  case object StartSchedules
}
