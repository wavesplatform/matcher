package com.wavesplatform.dex.model

import akka.actor.{ActorRef, ActorSystem, Props}
import com.wavesplatform.dex.actors.address.AddressActor.BlockchainInteraction
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.db.{EmptyOrderDB, TestOrderDB}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import com.wavesplatform.dex.time.Time

import scala.collection.mutable
import scala.concurrent.Future

class OrderHistoryStub(system: ActorSystem, time: Time, maxActiveOrders: Int, maxFinalizedOrders: Int) {

  implicit private val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

  private val refs = mutable.AnyRefMap.empty[Address, ActorRef]
  private val orders = mutable.AnyRefMap.empty[ByteStr, Address]

  private val emptyAddressBalanceUpdatesF = Future.successful(AddressBalanceUpdates.empty)

  private val blockchainInteraction = new BlockchainInteraction {
    override def getFullBalances(address: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates] = emptyAddressBalanceUpdatesF
  }

  def createAddressActor(address: Address, started: Boolean): Props =
    Props(
      new AddressActor(
        address,
        time,
        new TestOrderDB(maxFinalizedOrders),
        (_, _) => Future.successful(Right(())),
        e => Future.successful(Some(ValidatedCommandWithMeta(0L, 0, e))),
        started,
        blockchainInteraction,
        AddressActor.Settings.default.copy(maxActiveOrders = maxActiveOrders)
      )
    )

  private def actorFor(ao: AcceptedOrder): ActorRef =
    refs.getOrElseUpdate(
      ao.order.sender,
      system.actorOf(createAddressActor(ao.order.sender, started = true))
    )

  lazy val addressDir = system.actorOf(
    Props(
      new AddressDirectoryActor(
        EmptyOrderDB,
        createAddressActor,
        None,
        recovered = true
      )
    )
  )

  def ref(sender: Address): ActorRef = refs(sender)
  def ref(orderId: ByteStr): ActorRef = refs(orders(orderId))

  def process(event: Events.Event): Unit = event match {
    case oa: Events.OrderAdded =>
      orders += oa.order.order.id() -> oa.order.order.sender
      actorFor(oa.order) ! oa

    case ox: Events.OrderExecuted =>
      orders += ox.submitted.order.id() -> ox.submitted.order.sender
      orders += ox.counter.order.id() -> ox.counter.order.sender
      actorFor(ox.counter) ! ox
      actorFor(ox.submitted) ! ox

    case oc: Events.OrderCanceled => actorFor(oc.acceptedOrder) ! oc
  }

  def processAll(events: Events.Event*): Unit = events.foreach(process)
}
