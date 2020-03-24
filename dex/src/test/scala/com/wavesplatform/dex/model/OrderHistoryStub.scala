package com.wavesplatform.dex.model

import akka.actor.{ActorRef, ActorSystem, Props}
import com.wavesplatform.dex.db.{EmptyOrderDB, TestOrderDB}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.{AddressActor, AddressDirectory, SpendableBalancesActor}

import scala.collection.mutable
import scala.concurrent.Future

class OrderHistoryStub(system: ActorSystem, time: Time) {

  private implicit val efc: ErrorFormatterContext = (_: Asset) => 8

  private val refs   = mutable.AnyRefMap.empty[Address, ActorRef]
  private val orders = mutable.AnyRefMap.empty[ByteStr, Address]

  private val spendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]] = (_, _) => Future.successful(Map.empty[Asset, Long])
  private val allAssetsSpendableBalances: Address => Future[Map[Asset, Long]]      = _ => Future.successful(Map.empty[Asset, Long])

  private val spendableBalanceActor = system.actorOf(Props(new SpendableBalancesActor(spendableBalances, allAssetsSpendableBalances, addressDir)))

  def createAddressActor(address: Address, enableSchedules: Boolean): Props = {
    Props(
      new AddressActor(
        address,
        time,
        new TestOrderDB(100),
        _ => Future.successful(false),
        e => Future.successful { Some(QueueEventWithMeta(0, 0, e)) },
        _ => OrderBookAggregatedSnapshot.empty,
        enableSchedules,
        spendableBalanceActor,
        _ => Future.successful(8)
      )
    )
  }

  private def actorFor(ao: AcceptedOrder): ActorRef =
    refs.getOrElseUpdate(
      ao.order.sender,
      system.actorOf(createAddressActor(ao.order.sender, true))
    )

  lazy val addressDir = system.actorOf(
    Props(
      new AddressDirectory(
        EmptyOrderDB,
        createAddressActor,
        None
      )
    )
  )

  def ref(sender: Address): ActorRef  = refs(sender)
  def ref(orderId: ByteStr): ActorRef = refs(orders(orderId))

  def process(event: Events.Event): Unit = event match {
    case oa: Events.OrderAdded =>
      orders += oa.order.order.id() -> oa.order.order.sender
      actorFor(oa.order) ! oa

    case ox: Events.OrderExecuted =>
      orders += ox.submitted.order.id() -> ox.submitted.order.sender
      orders += ox.counter.order.id()   -> ox.counter.order.sender
      actorFor(ox.counter) ! ox
      actorFor(ox.submitted) ! ox

    case oc: Events.OrderCanceled => actorFor(oc.acceptedOrder) ! oc
  }

  def processAll(events: Events.Event*): Unit = events.foreach(process)
}
