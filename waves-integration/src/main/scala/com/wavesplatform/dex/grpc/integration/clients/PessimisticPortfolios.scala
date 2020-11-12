package com.wavesplatform.dex.grpc.integration.clients

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.instances.list._
import cats.instances.long._
import cats.instances.set._
import cats.syntax.foldable._
import cats.syntax.group._
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

import scala.collection.mutable

private[clients] class PessimisticPortfolios extends ScorexLogging {

  private val reentrantLock = new ReentrantReadWriteLock()

  private def read[T](f: => T): T =
    try { reentrantLock.readLock().lock(); f }
    finally reentrantLock.readLock().unlock()

  private def write[T](f: => T): T =
    try { reentrantLock.writeLock().lock(); f }
    finally reentrantLock.writeLock().unlock()

  // Longs are negative in both maps, see getPessimisticPortfolio
  private val portfolios = new mutable.AnyRefMap[Address, Map[Asset, Long]]()
  private val txs = new mutable.AnyRefMap[ByteString, Map[Address, Map[Asset, Long]]] // TODO id <------------

  def replaceWith(setTxs: Set[UtxTransaction]): Set[Address] = write {
    val setTxMap = setTxs.map(tx => tx.id -> tx).toMap
    val oldTxIds = txs.keySet.toSet -- setTxMap.keySet
    val newTxIds = setTxMap.keySet -- txs.keySet

    val newTxsPortfolios = newTxIds.toList.map(id => id -> getPessimisticPortfolio(setTxMap(id))) // TODO apply
    newTxsPortfolios.foreach(Function.tupled(txs.put))

    val addPortfolios = newTxsPortfolios.foldMap(_._2)
    val subtractPortfolios = oldTxIds.toList.foldMap(txs.remove(_).getOrElse(Map.empty))
    val diff = addPortfolios |-| subtractPortfolios
    diff.foreach { case (address, diff) =>
      portfolios.updateWith(address) { prev =>
        (prev.getOrElse(Map.empty) |+| diff)
          .filter(_._2 < 0) // TODO: guess it is not possible, but...
          .some
      }
    }
    log.info(s"replaceWith ids=${setTxMap.keySet.map(_.toVanilla)}, diff=$diff, txs=$setTxs")
    diff.keySet
  }

  // TODO
  def addAndRemove(addTxs: Seq[UtxTransaction], removeTxs: Seq[ByteString]): Set[Address] = write {
    log.info(s"addTxs: ${addTxs.map(_.id.toVanilla)}, removeTxs: ${removeTxs.map(_.toVanilla)}")
    addTxs.toList.foldMapK[Set, Address](addUnsafe) |+|
    removeTxs.toList.foldMapK[Set, Address](removeUnsafe)
  }

  private def addUnsafe(tx: UtxTransaction): Set[Address] = {
    val id = tx.id
    if (txs.contains(id)) {
      log.info(s"addUnsafe: already has ${id.toVanilla}")
      Set.empty
    } else if (tx.transaction.flatMap(_.transaction).fold(false)(_.data.isExchange)) {
      log.info(s"addUnsafe: ignoring because it is an exchange tx ${id.toVanilla}")
      Set.empty
    } else {
      val finalP = getPessimisticPortfolio(tx)
      log.info(s"addUnsafe: id=${id.toVanilla}, diff=$finalP, tx=$tx")
      // TODO we calculate and check only in the and?
      if (txs.put(id, finalP).isEmpty) {
        finalP.foreach {
          case (address, p) => portfolios.updateWith(address)(prev => (prev.getOrElse(Map.empty) |+| p).some)
        }
        finalP.keySet
      } else Set.empty
    }
  }

  private def removeUnsafe(txId: ByteString): Set[Address] =
    txs.remove(txId) match {
      case None =>
        log.info(s"removeUnsafe: wasn't id=${txId.toVanilla}")
        Set.empty[Address]
      case Some(p) =>
        log.info(s"removeUnsafe: id=${txId.toVanilla}, diff=$p")
        p.foreach {
          case (address, p) =>
            portfolios.updateWith(address) { prev =>
              prev.map(_ |-| p) // TODO cleanup. sometimes?
            }
        }
        p.keySet
    }

  def getAggregated(address: Address): Map[Asset, Long] = read(portfolios.getOrElse(address, Map.empty))

  // Utility

  def getPessimisticPortfolio(tx: UtxTransaction): Map[Address, Map[Asset, Long]] = tx.diff.flatMap(_.stateUpdate)
    .fold(Map.empty[Address, Map[Asset, Long]]) { diff =>
      // Balances
      val p1 = diff.balances.groupBy(_.address).map {
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
      val finalP = diff.leases.foldLeft(p1) {
        case (r, x) =>
          if (x.out <= 0) r // pessimistic
          else {
            val address = x.address.toVanillaAddress
            val orig = r.getOrElse(address, Map.empty)
            val updated = orig.updated(Waves, orig.getOrElse(Waves, 0L) - x.out)
            r.updated(address, updated)
          }
      }

      finalP
    }

}
