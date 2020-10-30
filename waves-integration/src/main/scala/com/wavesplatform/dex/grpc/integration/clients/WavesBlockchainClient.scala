package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

import cats.Group
import cats.instances.long._
import cats.instances.map.catsKernelStdMonoidForMap
import cats.syntax.group._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.DefaultWavesBlockchainClient.PessimisticPortfolios
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxEvent.Type
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import monix.reactive.Observable
import DefaultWavesBlockchainClient._
import cats.syntax.option._
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainData.ChangedAddresses
import com.wavesplatform.dex.grpc.integration.clients.state.{BlockInfo, BlockchainData, BlockchainEvent, BlockchainState, StateTransitions}
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object WavesBlockchainClient {

  case class Updates(updatedBalances: Map[Address, Map[Asset, Long]])

}

trait WavesBlockchainClient {
  def updates: Observable[WavesBlockchainClient.Updates]

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]

  def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[Boolean]

  def forgedOrder(orderId: ByteStr): Future[Boolean]

  def getNodeAddress: Future[InetAddress]

  def close(): Future[Unit]
}

class DefaultWavesBlockchainClient(
  meClient: MatcherExtensionClient[Future],
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext)
    extends WavesBlockchainClient {

  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  private val currentMaxHeight = new AtomicInteger(0)
  private val isRollback = new AtomicBoolean(false)

  private val emptyBalances: Balances = Map.empty
  private val knownBalances: AtomicReference[Balances] = new AtomicReference(emptyBalances)
  private val storingChangesDuringRollback: AtomicReference[Balances] = new AtomicReference(emptyBalances)

  private val pessimisticPortfolios = new PessimisticPortfolios

  private val dataUpdates = ConcurrentSubject.publish[BlockchainEvent]

  // TODO replace with deepReplace ?
  private def balanceUpdates(stateUpdate: StateUpdate): Balances =
    stateUpdate.balances.foldLeft(emptyBalances) {
      case (r, x) =>
        x.amount.fold(r) { assetAmount =>
          val address = x.address.toVanillaAddress
          val updated = r
            .getOrElse(address, Map.empty)
            .updated(assetAmount.assetId.toVanillaAsset, assetAmount.amount)
          r.updated(address, updated)
        }
    }

  private def leaseUpdates(stateUpdate: StateUpdate): Leases =
    stateUpdate.leases.foldLeft[Leases](Map.empty) { case (r, x) =>
      if (x.out <= 0) r
      else r.updated(x.address.toVanillaAddress, x.out)
    }

  override lazy val updates: Observable[Updates] = {
    val bState = Observable.fromFuture(meClient.currentBlockInfo).flatMap { startBlockInfo =>
      val start = startBlockInfo.height - MaxRollbackHeight - 1
      currentMaxHeight.set(start)
      val safeStream = bClient.blockchainEvents(start).onErrorRecoverWith {
        case _ => bClient.blockchainEvents(currentMaxHeight.get())
      }

      val eventsStream = safeStream
        .map { event =>
          val blockInfo = BlockInfo(event.height, event.id.toVanilla)
          event.update match {
            case Update.Empty => none // Nothing to do
            case Update.Append(updates) =>
              val regularBalanceChanges = updates.stateUpdate.fold(emptyBalances)(balanceUpdates)
              val outLeasesChanges = updates.stateUpdate.fold(Map.empty[Address, Long])(leaseUpdates)

              BlockchainEvent.Append(BlockchainData(
                blockInfo = blockInfo,
                changedAddresses = ChangedAddresses(blockInfo.height, regularBalanceChanges.keySet ++ outLeasesChanges.keySet) :: Nil,
                regularBalances = regularBalanceChanges,
                outLeases = outLeasesChanges
              )).some

            case Update.Rollback(value) =>
              value.`type` match {
                case RollbackType.BLOCK => BlockchainEvent.Rollback(blockInfo).some
                case RollbackType.MICROBLOCK => none // TODO ???
                case RollbackType.Unrecognized(_) => none // TODO ???
              }
          }
        }
        .collect { case Some(x) => x }

      val init = BlockchainState.Normal(
        BlockchainData(
          blockInfo = startBlockInfo,
          changedAddresses = List.empty,
          regularBalances = Map.empty,
          outLeases = Map.empty
        )
      )

      Observable(eventsStream, dataUpdates).merge.scan[BlockchainState](init) { (r, event) =>
        StateTransitions(r, event)
      }
    }

    val meStream = meClient.utxEvents.map { event =>
      event.`type` match {
        case Type.Switch(event) => event.transaction.map()
        case Type.Update(event) =>
        case Type.Empty =>
      }
    }

    Observable(bState, meStream).merge.map(Updates)
  }

  // TODO knownBalances
  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    meClient.spendableBalances(address, assets)

  // TODO knownBalances
  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address)

  // TODO get all and track in bClient
  override def isFeatureActivated(id: Short): Future[Boolean] =
    meClient.isFeatureActivated(id)

  // TODO track?
  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] =
    meClient.assetDescription(asset)

  // TODO track?
  override def hasScript(asset: IssuedAsset): Future[Boolean] = meClient.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] =
    meClient.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] =
    meClient.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] =
    meClient.runScript(address, input)

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] =
    meClient.wereForged(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean] =
    meClient.broadcastTx(tx)

  override def forgedOrder(orderId: ByteStr): Future[Boolean] =
    meClient.forgedOrder(orderId)

  override def getNodeAddress: Future[InetAddress] =
    meClient.getNodeAddress

  override def close(): Future[Unit] = meClient.close().zip(bClient.close()).map(_ => ())

  private def upsert(orig: Balances, update: Balances): Balances =
    update.foldLeft(orig) { case (r, (address, balances)) =>
      val orig = r.getOrElse(address, Map.empty)
      r.updated(address, orig ++ balances)
    }

}

object DefaultWavesBlockchainClient {

  val MaxRollbackHeight = 100

  private class PessimisticPortfolios {

    private type Portfolios = Map[Address, Map[Asset, Long]]

    private val portfolios = new ConcurrentHashMap[Address, Map[Asset, Long]]()
    private val txs = TrieMap.empty[UtxTransaction, Portfolios] // TODO Use ID

    def add(tx: UtxTransaction): Unit = tx.diff.flatMap(_.stateUpdate).foreach { diff =>
      // Balances
      val p1: Portfolios = diff.balances.groupBy(_.address).map {
        case (address, updates) =>
          val balances = updates.view
            .flatMap(_.amount)
            .collect {
              case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // pessimistic
            }
            .toMap
          address.toVanillaAddress -> balances
      }

      // Leasing
      val finalP: Portfolios = diff.leases.foldLeft(p1) {
        case (r, x) =>
          if (x.out <= 0) r // pessimistic
          else {
            val address = x.address.toVanillaAddress
            val orig = r.getOrElse(address, Map.empty)
            val updated = orig.updated(Waves, orig.getOrElse(Waves, 0L) - x.out)
            r.updated(address, updated)
          }
      }

      if (txs.putIfAbsent(tx, finalP).isEmpty)
        finalP.foreach {
          case (address, balances) => portfolios.compute(address, (_, orig) => orig |+| balances)
        }
    }

    def remove(tx: UtxTransaction): Unit = txs.remove(tx).foreach { p =>
      p.foreach {
        case (address, balances) => portfolios.compute(address, (_, orig) => orig |-| balances)
      }
    }

    def getAggregated(address: Address): Map[Asset, Long] = portfolios.get(address)

  }

}
