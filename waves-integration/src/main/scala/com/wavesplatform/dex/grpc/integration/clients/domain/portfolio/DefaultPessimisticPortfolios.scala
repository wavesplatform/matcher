package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long._
import cats.instances.set._
import cats.instances.tuple._
import cats.syntax.foldable._
import cats.syntax.group._
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup

import scala.collection.mutable

class DefaultPessimisticPortfolios() extends PessimisticPortfolios {
  // Longs are negative in both maps, see getPessimisticPortfolio
  private val portfolios = new mutable.AnyRefMap[Address, Map[Asset, Long]]()
  private val txs = new mutable.AnyRefMap[ByteString, Map[Address, Map[Asset, Long]]]

  def this(initPortfolios: AddressAssets, initTxs: Map[ByteString, AddressAssets]) = {
    this()
    initPortfolios.foreach(Function.tupled(portfolios.put))
    initTxs.foreach(Function.tupled(txs.put))
  }

  override def getAggregated(address: Address): Map[Asset, Long] = portfolios.getOrElse(address, Map.empty)

  override def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address] = {
    val origTxIds = txs.keySet.toSet
    val setTxMap = setTxs.map(x => x.txId -> x.pessimisticPortfolio).toMap
    val setTxIds = setTxMap.keySet

    val putTxIds = setTxIds -- origTxIds
    val removeTxIds = origTxIds -- setTxIds

    if (removeTxIds.isEmpty && putTxIds.isEmpty) Set.empty
    else {
      // It is safe to use setTxMap.apply here, because putTxIds contains elements only from setTxIds
      val putTxsPortfolios = putTxIds.toList.map(id => id -> setTxMap(id))
      val addPortfolios = putTxsPortfolios.foldMap { case (id, p) =>
        txs.put(id, p)
        p
      }

      val subtractPortfolios = removeTxIds.toList.foldMap(txs.remove(_).getOrElse(Map.empty))

      val diff = addPortfolios |-| subtractPortfolios
      diff.foreach { case (address, diff) =>
        portfolios.updateWith(address) { prev =>
          (prev.getOrElse(Map.empty) |+| diff)
            // .filter(_._2 < 0) // Don't need, because transactions of addPortfolios and subtractPortfolios don't interfere
            .some
        }
      }
      diff.keySet
    }
  }

  override def addPending(txs: Seq[PessimisticTransaction]): Set[Address] =
    txs.toList.foldMapK[Set, Address](addUnsafe)

  /**
   * @return (affected addresses, unknown transactions)
   */
  override def processForged(txIds: Seq[ByteString]): (Set[Address], List[ByteString]) =
    txIds.toList.foldMap { txId =>
      val (known, affected) = removeUnsafe(txId)
      (affected, if (known) Nil else List(txId))
    }

  override def removeFailed(txIds: Seq[ByteString]): Set[Address] = ???

  private def addUnsafe(tx: PessimisticTransaction): Set[Address] = {
    val id = tx.txId
    if (txs.contains(id)) Set.empty
    else {
      val finalP = tx.pessimisticPortfolio
      // TODO we calculate and check only in the and?
      if (txs.put(id, finalP).isEmpty) {
        finalP.foreach {
          case (address, p) => portfolios.updateWith(address)(_.foldLeft(p)(_ |+| _).some)
        }
        finalP.keySet
      } else Set.empty
    }
  }

  /**
   * @return (known?, affected addresses)
   */
  private def removeUnsafe(txId: ByteString): (Boolean, Set[Address]) = {
    val tx = txs.remove(txId)
    val affectedAddresses = tx match {
      case None => Set.empty[Address]
      case Some(p) =>
        p.foreach {
          case (address, p) =>
            portfolios.updateWith(address) { prev =>
              prev.map(xs => (xs |-| p).filter(_._2 < 0)).filter(_.nonEmpty)
            }
        }
        p.keySet
    }
    (tx.isDefined, affectedAddresses)
  }

}
