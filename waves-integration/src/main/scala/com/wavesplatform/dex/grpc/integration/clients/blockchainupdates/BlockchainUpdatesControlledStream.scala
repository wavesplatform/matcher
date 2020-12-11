package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import cats.syntax.option._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.observers.IntegrationObserver
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import io.grpc.stub.ClientCalls
import io.grpc.{CallOptions, Grpc, ManagedChannel}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.util.chaining._

// TODO DEX-999
class BlockchainUpdatesControlledStream(channel: ManagedChannel)(implicit scheduler: Scheduler)
    extends ControlledStream[SubscribeEvent]
    with ScorexLogging {
  @volatile private var grpcObserver: Option[IntegrationObserver[SubscribeEvent]] = None

  private val internalStream = ConcurrentSubject.publish[SubscribeEvent]
  override val stream: Observable[SubscribeEvent] = internalStream

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent]
  override val systemStream: Observable[SystemEvent] = internalSystemStream

  def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")

    val call = channel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001
    val observer = new IntegrationObserver[SubscribeEvent](
      internalStream,
      { () =>
        log.info(s"Getting blockchain events from ${call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)} starting from $height")
        internalSystemStream.onNext(ControlledStream.SystemEvent.BecameReady)
      },
      { e =>
        log.warn(s"Got an error in blockchain events", e)
        internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
      }
    ).tap(x => grpcObserver = x.some)

    ClientCalls.asyncServerStreamingCall(call, new SubscribeRequest(height), observer)
  }

  def requestNext(): Unit = grpcObserver.foreach(_.requestNext())

  override def stop(): Unit = {
    log.info("Stopping balance updates stream")
    stopGrpcObserver()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info("Closing balance updates stream")
    stopGrpcObserver()
    internalStream.onComplete()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Closed)
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.onCompleted())
    grpcObserver = None
  }

}
