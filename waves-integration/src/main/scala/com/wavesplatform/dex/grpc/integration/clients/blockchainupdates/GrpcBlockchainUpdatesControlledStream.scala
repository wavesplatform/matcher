package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import cats.syntax.option._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.clients.domain.BlockRef
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.observers.IntegrationObserver
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import io.grpc.stub.ClientCalls
import io.grpc.{CallOptions, ClientCall, Grpc, ManagedChannel}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

// TODO DEX-999
/*
  From the docs of reactive streams: the grammar must still be respected: (onNext)* (onComplete | onError)
  On error we just restart the stream, so r receives updates from a new stream. That is why we don't propagate errors to r
 */
class GrpcBlockchainUpdatesControlledStream(channel: ManagedChannel)(implicit scheduler: Scheduler)
    extends BlockchainUpdatesControlledStream
    with ScorexLogging {

  private val logPrefix = s"[${hashCode()}]" // TODO remove in future versions

  @volatile private var grpcObserver: Option[BlockchainUpdatesObserver] = None

  private val internalStream = ConcurrentSubject.publish[SubscribeEvent]
  override val stream: Observable[SubscribeEvent] = internalStream

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent]
  override val systemStream: Observable[SystemEvent] = internalSystemStream

  override def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")
    log.info(s"$logPrefix Connecting to Blockchain events stream, getting blocks from $height")

    val call = channel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001
    val observer = new BlockchainUpdatesObserver(call, height)
    grpcObserver = observer.some
    ClientCalls.asyncServerStreamingCall(call, new SubscribeRequest(height), observer)
  }

  override def requestNext(): Unit = grpcObserver.foreach(_.requestNext())

  override def stop(): Unit = if (grpcObserver.nonEmpty) {
    log.info(s"$logPrefix Stopping balance updates stream")
    stopGrpcObserver()
    internalSystemStream.onNext(SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info(s"$logPrefix Closing balance updates stream")
    stopGrpcObserver()
    internalStream.onComplete()
    internalSystemStream.onNext(SystemEvent.Closed)
    internalSystemStream.onComplete()
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.close())
    grpcObserver = None
  }

  private class BlockchainUpdatesObserver(call: ClientCall[SubscribeRequest, SubscribeEvent], startHeight: Int)
      extends IntegrationObserver[SubscribeRequest, SubscribeEvent](internalStream) {

    override def onReady(): Unit = {
      internalSystemStream.onNext(SystemEvent.BecameReady)
      val address = Option(call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)).fold("unknown")(_.toString)
      log.info(s"$logPrefix Getting blockchain events from $address starting from $startHeight")
    }

    override def onNext(value: SubscribeEvent): Unit = {
      def message = {
        val update = value.getUpdate
        def ref = BlockRef(update.height, update.id.toVanilla)
        update.update match {
          case Update.Empty => "empty"
          case Update.Rollback(x) => s"rollback tpe=${x.`type`}, $ref"
          case Update.Append(x) =>
            val tpe = x.body match {
              case Body.Empty => "empty"
              case _: Body.Block => "f"
              case _: Body.MicroBlock => "m"
            }
            s"append tpe=$tpe, $ref"
        }
      }
      log.debug(s"$logPrefix Got $message")
      super.onNext(value)
    }

    override def onError(e: Throwable): Unit =
      if (isClosed) log.trace(s"$logPrefix Got an expected error during closing: ${Option(e.getMessage).getOrElse("null")}")
      else {
        log.warn(s"$logPrefix Got an error in blockchain events", e)
        internalSystemStream.onNext(SystemEvent.Stopped)
      }

    override def onCompleted(): Unit = log.error(s"$logPrefix Unexpected onCompleted")
  }

}
