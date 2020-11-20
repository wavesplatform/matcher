package com.wavesplatform.dex.grpc.integration.clients

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import cats.syntax.option._
import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.collection.MapOps.{Ops, Ops2}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.BlockchainUpdatesClient.{BlockchainUpdatesControl, RestartException}
import com.wavesplatform.dex.grpc.integration.clients.state._
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver, StreamObserver}
import io.grpc.{ManagedChannel, Status, StatusRuntimeException}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Observable, Observer}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

trait BlockchainUpdatesClient {
  def blockchainEvents(fromHeight: Int): (Observable[WavesNodeEvent], BlockchainUpdatesControl)
  def close(): Future[Unit]
}

object BlockchainUpdatesClient {
  // TODO move to the base interface?
  class RestartException(val fromHeight: Int) extends RuntimeException(s"Force restart from $fromHeight")

  trait BlockchainUpdatesControl {
    def restartFrom(height: Int): Unit
    def checkpoint(height: Int): Unit
    def stop(): Unit
  }

}

class DefaultBlockchainUpdatesClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)(implicit
  grpcExecutionContext: ExecutionContext
) extends BlockchainUpdatesClient
    with ScorexLogging {
  private val shuttingDown = new AtomicBoolean(false)
  private val grpcService = BlockchainUpdatesApiGrpc.stub(channel) // TODO move this dependency to constructor?

  override def blockchainEvents(fromHeight: Int): (Observable[WavesNodeEvent], BlockchainUpdatesControl) = {
    /* From the docs: the grammar must still be respected: (onNext)* (onComplete | onError)
     * That is why we don't propagate errors to r */
    val subject = ConcurrentSubject.publish[WavesNodeEvent](monixScheduler)
    val control = new DefaultBlockchainUpdatesClientControl(
      subject,
      subscribe = { (subject, fromHeight, onError) =>
        if (shuttingDown.get) none
        else {
          log.debug(s"Subscribed on blockchain events from $fromHeight")
          val r = new EventsObserver(subject, onError)
          grpcService.subscribe(new SubscribeRequest(fromHeight), r)
          r.some
        }
      }
    )(monixScheduler)
    control.startFrom(fromHeight)
    (subject, control)
  }

  override def close(): Future[Unit] = {
    shuttingDown.set(true)
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)
    // See NettyChannelBuilder.eventLoopGroup
    // TODO can we have one loop group for both clients?
    eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

  final private class EventsObserver(subject: Observer[WavesNodeEvent], doOnError: Throwable => Unit)
      extends ClientResponseObserver[Empty, SubscribeEvent]
      with AutoCloseable {

    private var requestStream: ClientCallStreamObserver[Empty] = _

    override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = this.requestStream = requestStream

    override def onNext(value: SubscribeEvent): Unit = {
      log.trace(s"Got $value")
      value.update.flatMap(toEvent).foreach(subject.onNext)
    }

    override def onError(e: Throwable): Unit = {
      log.warn("Got an error", e)
      if (!shuttingDown.get()) doOnError(e)
    }

    override def close(): Unit = {
      log.info("Closed")
      if (requestStream != null) {
        requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))
        requestStream = null
      }
    }

    override def onCompleted(): Unit = {
      log.info("gRPC balance changes stream completed!")
      if (requestStream != null) requestStream.onCompleted()
    }

  }

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val emptyBalances: Balances = Map.empty

  /**
   * Cases:
   * 1. Downloading blocks: Append+
   * 2. Appending on a network's height: AppendMicro*, RollbackMicro?, Append
   * 2. Rollback: Rollback, Append+
   */
  private def toEvent(event: BlockchainUpdated): Option[WavesNodeEvent] = {
    val blockRef = BlockRef(event.height, event.id.toVanilla)
    event.update match {
      case Update.Empty => none // Nothing to do
      case Update.Append(updates) =>
        log.info(s"toEvent.stateUpdate: ${updates.stateUpdate}")
        val regularBalanceChanges = balanceUpdates(updates.stateUpdate).deepReplace(balanceUpdates(updates.transactionStateUpdates))
        val outLeasesChanges = leaseUpdates(updates.stateUpdate).deepCombine(leaseUpdates(updates.transactionStateUpdates))((_, x) => x)
        log.info(s"toEvent.regularBalanceChanges: $regularBalanceChanges")

        val blockInfo = updates.body match {
          case Body.Empty => none // Log
          case Body.Block(block) => (WavesBlock.Type.Block, block.block.get.header.get.reference.toVanilla).some
          case Body.MicroBlock(block) => (WavesBlock.Type.MicroBlock, block.microBlock.get.microBlock.get.reference.toVanilla).some
        }

        blockInfo.map { case (tpe, reference) =>
          val block = WavesBlock(
            ref = blockRef,
            reference = reference,
            changes = BlockchainBalance(regularBalanceChanges, outLeasesChanges),
            tpe = tpe
          )

          WavesNodeEvent.Appended(block, updates.transactionIds)
        }

      case Update.Rollback(value) =>
        value.`type` match {
          case RollbackType.BLOCK | RollbackType.MICROBLOCK => WavesNodeEvent.RolledBackTo(blockRef).some
          case RollbackType.Unrecognized(_) => none // TODO ???
        }
    }
  }

  // TODO replace with deepReplace ?
  private def balanceUpdates(stateUpdate: Iterable[StateUpdate]): Balances =
    stateUpdate.flatMap(_.balances).foldLeft(emptyBalances) {
      case (r, x) =>
        // TODO what if absent? All assets has gone?
        x.amount.fold(r) { assetAmount =>
          val address = x.address.toVanillaAddress
          val updated = r
            .getOrElse(address, Map.empty)
            .updated(assetAmount.assetId.toVanillaAsset, assetAmount.amount)
          log.info(s"balanceUpdates: $address: ${assetAmount.assetId.toVanillaAsset} -> ${assetAmount.amount}, updated: $updated")
          r.updated(address, updated)
        }
    }

  private def leaseUpdates(stateUpdate: Iterable[StateUpdate]): Leases =
    stateUpdate.flatMap(_.leases).foldLeft[Leases](Map.empty) { case (r, x) =>
      r.updated(x.address.toVanillaAddress, x.out)
    }

}

final private class DefaultBlockchainUpdatesClientControl(
  subject: Observer[WavesNodeEvent],
  subscribe: (Observer[WavesNodeEvent], Int, Throwable => Unit) => Option[StreamObserver[SubscribeEvent]]
)(implicit scheduler: Scheduler)
    extends BlockchainUpdatesControl
    with ScorexLogging {
  @volatile private var grpcObserver: Option[StreamObserver[SubscribeEvent]] = None
  @volatile private var checkpointHeight = 1 // It is not processed height! A less value could be emitted to control

  def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")
    checkpointHeight = height - 1
    grpcObserver = subscribe(subject, height, doOnError)
  }

  override def restartFrom(height: Int): Unit = {
    stopGrpcObserver()
    startFrom(height)
  }

  override def checkpoint(height: Int): Unit = checkpointHeight = height

  override def stop(): Unit = {
    log.info("Stopping balance updates stream")
    stopGrpcObserver()
    subject.onComplete()
  }

  private def doOnError(e: Throwable): Unit = {
    log.warn(s"Got an error in blockchain events", e)
    val fromHeight = e match {
      case e: RestartException =>
        stopGrpcObserver()
        e.fromHeight
      case _ => // Already stopped
        math.max(1, checkpointHeight - 1) // TODO ???
    }
    scheduler.scheduleOnce(100.millis) { // TODO to config
      subject.onNext(WavesNodeEvent.SyncFailed(fromHeight))
      startFrom(fromHeight)
    }
  }

  private def stopGrpcObserver(): Unit = grpcObserver.foreach(_.onCompleted())

}
