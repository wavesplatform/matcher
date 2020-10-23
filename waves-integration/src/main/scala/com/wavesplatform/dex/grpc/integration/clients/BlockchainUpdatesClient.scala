package com.wavesplatform.dex.grpc.integration.clients

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import cats.kernel.Monoid
import com.google.protobuf.empty.Empty
import java.net.InetAddress
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.MatcherExtensionClient.BalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.dex.grpc.integration.exceptions.{UnexpectedConnectionException, WavesNodeConnectionLostException}
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.RunScriptResponse.Result
import com.wavesplatform.dex.grpc.integration.services._
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{ManagedChannel, Status, StatusRuntimeException}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.state.Portfolio
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.MatcherExtensionClient.BalanceChanges
import com.wavesplatform.dex.grpc.integration.services.{BalanceChangesFlattenResponse, WavesBlockchainApiGrpc}
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.protobuf.transaction.Transaction
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{ManagedChannel, Status, StatusRuntimeException}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

trait BlockchainUpdatesClient {
  def blockchainUpdates: Observable[Map[Address, Map[Asset, Long]]]
  def close(): Future[Unit]
}

class DefaultBlockchainUpdatesClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)
    extends BlockchainUpdatesClient
with ScorexLogging {
  private type AddressesAssetsChanges = Map[Address, Set[Asset]]

  private val shuttingDown = new AtomicBoolean(false)
  private val grpcService = BlockchainUpdatesApiGrpc.stub(channel) // TODO Constructo

  private val txAddressesAssetsChanges = new ConcurrentHashMap[Transaction, AddressesAssetsChanges]()
  private val currentMaxHeight = new AtomicInteger(context.blockchain.height)
  private val isRollback = new AtomicBoolean(false)
  private val storingChangesDuringRollback: AtomicReference[AddressesAssetsChanges] = new AtomicReference(Map.empty)

  private val blockchainBalanceUpdates = ConcurrentSubject.publish[SubscribeEvent](monixScheduler)

  grpcService.subscribe().blockchainUpdated.map {
    case BlockAppended(_, newHeight, _, _, _, transactionStateUpdates) =>
      val changes = getAddressesChangedAssets(transactionStateUpdates)
      if (isRollback.get()) {
        storingChangesDuringRollback.updateAndGet(_ |+| changes)
        if (newHeight != currentMaxHeight.get) emptyAddressAssetsChanges
        else {
          isRollback.set(false)
          storingChangesDuringRollback.getAndSet(Map.empty)
        }
      } else {
        currentMaxHeight.set(newHeight)
        changes
      }

    case MicroBlockAppended(_, _, _, _, transactionStateUpdates) => getAddressesChangedAssets(transactionStateUpdates)
    case MicroBlockRollbackCompleted(_, _) => emptyAddressAssetsChanges
    case RollbackCompleted(_, _) => isRollback.set(true); emptyAddressAssetsChanges
  }

  private def getAddressesChangedAssets(transactionStateUpdates: Seq[StateUpdate]): AddressesAssetsChanges =
    transactionStateUpdates.foldLeft(Map.empty[Address, Set[Asset]]) {
      case (result, stateUpdate) => result |+| stateUpdate.balances.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    }

  override def blockchainUpdates: Observable[Map[Address, Map[Asset, Long]]] = ???

  override def close(): Future[Unit] = ???

  private def requestRealTimeBalanceChanges(): Unit = {
    grpcService.subscribe(new SubscribeRequest(0), new RealTimeBalanceChangesObserver)
  }

  final private class RealTimeBalanceChangesObserver extends ClientResponseObserver[Empty, SubscribeEvent] with AutoCloseable {

    private var requestStream: ClientCallStreamObserver[Empty] = _

    override def onCompleted(): Unit = log.info("Balance changes stream completed!")

    override def onNext(value: SubscribeEvent): Unit = blockchainBalanceUpdates.onNext(value)

    override def onError(e: Throwable): Unit = if (!shuttingDown.get()) {
      channel.resetConnectBackoff()
      requestRealTimeBalanceChanges() // TODO Can we do this?
    }

    override def close(): Unit = if (requestStream != null) requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))

    override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = this.requestStream = requestStream
  }
}

object DefaultBlockchainUpdatesClient {}
