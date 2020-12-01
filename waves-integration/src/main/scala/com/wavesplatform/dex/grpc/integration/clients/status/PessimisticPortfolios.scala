package com.wavesplatform.dex.grpc.integration.clients.status

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
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import mouse.any._

import scala.collection.mutable

// TODO DEX-995
private[clients] class PessimisticPortfolios(ignoredExchangeTxSenderPublicKey: Option[ByteStr]) extends ScorexLogging {

  private val reentrantLock = new ReentrantReadWriteLock()

  private def read[T](f: => T): T =
    try { reentrantLock.readLock().lock(); f }
    finally reentrantLock.readLock().unlock()

  private def write[T](f: => T): T =
    try { reentrantLock.writeLock().lock(); f }
    finally reentrantLock.writeLock().unlock()

  // Longs are negative in both maps, see getPessimisticPortfolio
  private val portfolios = new mutable.AnyRefMap[Address, Map[Asset, Long]]()
  private val txs = new mutable.AnyRefMap[ByteString, Map[Address, Map[Asset, Long]]]

  // TODO DEX-995 Extract a class (e.g. decorator) with a cache functionality
  private val MaxForgedTransactions = 10000

  // TODO DEX-995 better data structure (or two!)
  private val forgedTxsCache: mutable.Queue[ByteString] = new mutable.Queue[ByteString](MaxForgedTransactions)

  // TODO DEX-995 Cleanup for forgedTxCache?
  // TODO DEX-995 An optimimization for case when setTxs == txs
  // TODO DEX-1013
  def replaceWith(setTxs: Seq[UtxTransaction]): Set[Address] = write {
    val setTxMap = setTxs.collect { case tx if !forgedTxsCache.contains(tx.id) => tx.id -> tx }.toMap
    val oldTxIds = txs.keySet.toSet -- setTxMap.keySet
    val newTxIds = setTxMap.keySet -- txs.keySet

    // TODO DEX-995 Is it safe to use setTxMap.apply?
    val newTxsPortfolios = newTxIds.toList.map(id => id -> getPessimisticPortfolio(setTxMap(id)))
    newTxsPortfolios.foreach(Function.tupled(txs.put))

    val addPortfolios = newTxsPortfolios.foldMap(_._2)
    val subtractPortfolios = oldTxIds.toList.foldMap(txs.remove(_).getOrElse(Map.empty))
    val diff = addPortfolios |-| subtractPortfolios
    diff.foreach { case (address, diff) =>
      portfolios.updateWith(address) { prev =>
        (prev.getOrElse(Map.empty) |+| diff)
          .filter(_._2 < 0) // TODO DEX-995: guess it is not possible, but...
          .some
      }
    }
    log.info(s"replaceWith ids=${setTxMap.keySet.map(_.toVanilla)}, diff=$diff, txs=$setTxs")
    diff.keySet
  }

  // TODO DEX-995 Also remove from the cache
  // TODO DEX-1013
  def processForged(txIds: Seq[ByteString]): Set[Address] = write {
    log.info(s"processForged: ${txIds.map(_.toVanilla)}")
    txIds.toList.foldMapK[Set, Address] { txId =>
      removeUnsafe(txId).unsafeTap { affected =>
        if (affected.isEmpty) { // We haven't seen this tx
          if (forgedTxsCache.size == MaxForgedTransactions) forgedTxsCache.removeLast()
          forgedTxsCache.addOne(txId)
        } // else forgedTxsCache.removeFirst(_ == txId) OR in addPending
      }
    }
  }

  // TODO DEX-995
  // TODO DEX-1013
  def removeFailed(): Set[Address] = ???

  // TODO DEX-1013
  def addPending(txs: Seq[UtxTransaction]): Set[Address] = write {
    val filtered = txs.filter(tx => !forgedTxsCache.contains(tx.id))
    log.info(s"addPending: ${filtered.map(_.id.toVanilla).mkString(", ")}; notFiltered=${txs.map(_.id.toVanilla)}")
    filtered.toList.foldMapK[Set, Address](addUnsafe)
  }

  private def addUnsafe(tx: UtxTransaction): Set[Address] = {
    val id = tx.id
    if (txs.contains(id)) {
      log.info(s"addUnsafe: already has ${id.toVanilla}")
      Set.empty
    } else tx.transaction.flatMap(_.transaction) match {
      case Some(tx) if tx.data.isExchange && ignoredExchangeTxSenderPublicKey.contains(tx.senderPublicKey.toVanilla) =>
        log.info(s"addUnsafe: ignoring because it is an exchange tx ${id.toVanilla}")
        Set.empty
      case _ =>
        val finalP = getPessimisticPortfolio(tx)
        log.info(s"addUnsafe: id=${id.toVanilla}, diff=$finalP, tx=$tx")
        // TODO we calculate and check only in the and?
        if (txs.put(id, finalP).isEmpty) {
          finalP.foreach {
            case (address, p) => portfolios.updateWith(address)(_.foldLeft(p)(_ |+| _).some)
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
              val r = prev.map(_ |-| p) // TODO DEX-995 We need a cleanup
              log.info(s"removeUnsafe of $address: $prev -> $r")
              r
            }
        }
        p.keySet
    }

  def getAggregated(address: Address): Map[Asset, Long] = read(portfolios.getOrElse(address, Map.empty))

  // Utility

  // TODO DEX-995 Could we do it faster?
  def getPessimisticPortfolio(tx: UtxTransaction): Map[Address, Map[Asset, Long]] = tx.diff.flatMap(_.stateUpdate)
    .fold(Map.empty[Address, Map[Asset, Long]]) { diff =>
      // Balances
      val p1 = diff.balances.groupBy(_.address).map {
        case (address, updates) =>
          val balances = updates.view
            .flatMap(_.amount)
            .collect {
              case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // Count only pessimistic
            }
            .toMap
          address.toVanillaAddress -> balances
      }

      // Leasing
      val finalP = diff.leases.foldLeft(p1) {
        case (r, x) =>
          if (x.out <= 0) r // Ignore an invalid values
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
