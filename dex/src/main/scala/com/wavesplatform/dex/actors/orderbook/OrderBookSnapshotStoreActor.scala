package com.wavesplatform.dex.actors.orderbook

import akka.actor.{Actor, Props}
import com.wavesplatform.dex.actors.orderbook.OrderBookSnapshotStoreActor._
import com.wavesplatform.dex.db.OrderBookSnapshotDb
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.model.OrderBookSnapshot
import com.wavesplatform.dex.queue.ValidatedCommandWithMeta.Offset

import scala.concurrent.Future
import scala.util.{Failure, Success}

class OrderBookSnapshotStoreActor(db: OrderBookSnapshotDb[Future]) extends Actor with ScorexLogging {

  import context.dispatcher

  override def receive: Receive = {
    case Message.GetSnapshot(p) =>
      val origSender = sender()
      db.get(p).onComplete {
        case Success(result) =>
          origSender ! Response.GetSnapshot(result)
        case Failure(th) =>
          log.error(s"error retrieving snapshot for asset pair: $p", th)
      }

    case Message.Update(p, offset, newSnapshot) =>
      val origSender = sender()
      db.update(p, offset, newSnapshot).onComplete {
        case Success(_) =>
          origSender ! Response.Updated(offset)
        case Failure(th) =>
          log.error(s"error while updating offset for asset pair: $p, offset: $offset", th)
      }

    case Message.Delete(p) =>
      val origSender = sender()
      db.delete(p).onComplete {
        case Success(_) =>
          origSender ! Response.Deleted(p)
        case Failure(th) =>
          log.error(s"error while deleting offset for asset pair: $p", th)
      }
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
    case class Updated(offset: Offset) extends Response
    case class Deleted(assetPair: AssetPair) extends Response
  }

  def props(db: OrderBookSnapshotDb[Future]): Props = Props(new OrderBookSnapshotStoreActor(db))
}
