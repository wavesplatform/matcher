package com.wavesplatform.dex.model

import akka.actor.{ActorRef, ActorSystem, Props}
import com.wavesplatform.dex.actors.address.AddressActor.BlockchainInteraction
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.db.{EmptyOrderDb, TestOrderDb}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.transaction.{ExchangeTransactionResult, ExchangeTransactionV2}
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta
import com.wavesplatform.dex.time.Time

import scala.collection.mutable
import scala.concurrent.Future

class OrderHistoryStub(system: ActorSystem, time: Time, maxActiveOrders: Int, maxFinalizedOrders: Int) {

  private val refs = mutable.AnyRefMap.empty[Address, ActorRef]
  private val orders = mutable.AnyRefMap.empty[ByteStr, Address]

  private val emptyAddressBalanceUpdatesF = Future.successful(AddressBalanceUpdates.empty)

  private val blockchainInteraction = new BlockchainInteraction {
    override def getFullBalances(address: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates] = emptyAddressBalanceUpdatesF
  }

  private def assetBriefInfo: Asset => BriefAssetDescription =
    asset => BriefAssetDescription(asset.toString, 2, hasScript = false, isNft = false)

  def createAddressActor(address: Address, recovered: Boolean): Props =
    Props(
      new AddressActor(
        address,
        time,
        TestOrderDb(maxFinalizedOrders),
        (_, _) => Future.successful(Right(())),
        e => Future.successful(Some(ValidatedCommandWithMeta(0L, 0, e))),
        recovered,
        blockchainInteraction,
        AddressActor.Settings.default.copy(maxActiveOrders = maxActiveOrders),
        assetBriefInfo
      )
    )

  private def actorFor(ao: AcceptedOrder): ActorRef = actorForAddress(ao.order.sender)

  private def actorForAddress(address: Address): ActorRef =
    refs.getOrElseUpdate(
      address,
      system.actorOf(createAddressActor(address, recovered = true))
    )

  lazy val addressDir = system.actorOf(
    Props(
      new AddressDirectoryActor(
        EmptyOrderDb(),
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
      actorFor(oa.order) ! AddressActor.Command.ApplyOrderBookAdded(oa)

    case ox: Events.OrderExecuted =>
      orders += ox.submitted.order.id() -> ox.submitted.order.sender
      orders += ox.counter.order.id() -> ox.counter.order.sender
      val command = AddressActor.Command.ApplyOrderBookExecuted(ox, mkExchangeTx(ox))
      List(ox.counter, ox.submitted).map(_.order.sender.toAddress).toSet.map(actorForAddress).foreach(_ ! command)

    case oc: Events.OrderCanceled =>
      actorFor(oc.acceptedOrder) ! AddressActor.Command.ApplyOrderBookCanceled(oc)
  }

  def processAll(events: Events.Event*): Unit = events.foreach(process)

  private def mkExchangeTx(oe: OrderExecuted): ExchangeTransactionResult[ExchangeTransactionV2] = {
    val (sellOrder, buyOrder) = if (oe.counter.isSellOrder) (oe.counter, oe.submitted) else (oe.submitted, oe.counter)
    ExchangeTransactionV2
      .create(
        buyOrder = buyOrder.order,
        sellOrder = sellOrder.order,
        amount = sellOrder.amount,
        price = sellOrder.price,
        buyMatcherFee = buyOrder.matcherFee,
        sellMatcherFee = sellOrder.matcherFee,
        fee = 300000L,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty
      )
  }

}
