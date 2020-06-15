package com.wavesplatform.dex.actors.orderbook

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.actors.orderbook.OrderBookSnapshotStoreActor._
import com.wavesplatform.dex.db.OrderBookSnapshotDB
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.QueueEventWithMeta.Offset

class OrderBookSnapshotStoreActor(db: OrderBookSnapshotDB) extends Actor {
  override def receive: Receive = {
    case Message.GetSnapshot(p) => sender() ! Response.GetSnapshot(db.get(p))

    case Message.Update(p, offset, newSnapshot) =>
      db.update(p, offset, newSnapshot)
      sender() ! Response.Updated(offset)

    case Message.Delete(p) => db.delete(p)
  }
}

object OrderBookSnapshotStoreActor {
  sealed trait Message
  object Message {
    case class GetSnapshot(assetPair: AssetPair) extends Message

    /**
      * @param newSnapshot None if it wasn't changed
      */
    case class Update(assetPair: AssetPair, offset: Offset, newSnapshot: Option[OrderBookSnapshot]) extends Message

    case class Delete(assetPair: AssetPair) extends Message
  }

  sealed trait Response
  object Response {
    case class GetSnapshot(result: Option[(Offset, OrderBookSnapshot)]) extends Response
    case class Updated(offset: Offset)                                  extends Response
    case class Deleted(assetPair: AssetPair)                            extends Response
  }

  def props(db: OrderBookSnapshotDB): Props = Props(new OrderBookSnapshotStoreActor(db))
}
