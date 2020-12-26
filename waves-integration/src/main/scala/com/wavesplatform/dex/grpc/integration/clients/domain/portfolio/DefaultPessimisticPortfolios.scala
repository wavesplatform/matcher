package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import alleycats.std.iterable._
import cats.instances.list._
import cats.instances.long._
import cats.instances.tuple._
import cats.kernel.Semigroup
import cats.syntax.foldable._
import cats.syntax.group._
import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup

import scala.collection.mutable
import scala.util.chaining._

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

  override def addPending(addTxs: Iterable[PessimisticTransaction]): Set[Address] = {
    val pessimisticChanges = addTxs
      .filterNot(addTxs => txs.contains(addTxs.txId))
      .foldMap[AddressAssets](tx => tx.pessimisticPortfolio.tap(txs.put(tx.txId, _)))

    pessimisticChanges.foreach { case (address, p) => portfolios.updateWith(address)(_.foldLeft(p)(Semigroup.combine).some) }
    pessimisticChanges.keySet
  }

  /**
   * @return (affected addresses, unknown transactions)
   */
  override def processConfirmed(txIds: Iterable[ByteString]): (Set[Address], List[ByteString]) = {
    val (pessimisticChangesToRevert, unknownTxIds) = txIds.foldMap[(AddressAssets, List[ByteString])] { txId =>
      txs.remove(txId).fold[(AddressAssets, List[ByteString])]((Map.empty, List(txId)))((_, Nil))
    }
    revert(pessimisticChangesToRevert)
    (pessimisticChangesToRevert.keySet, unknownTxIds)
  }

  override def removeFailed(txIds: Iterable[ByteString]): Set[Address] = {
    val pessimisticChangesToRevert = txIds.foldMap[AddressAssets](txs.remove(_).getOrElse(Map.empty))
    revert(pessimisticChangesToRevert)
    pessimisticChangesToRevert.keySet
  }

  private def revert(toRevert: AddressAssets): Unit = toRevert.foreach { case (address, toRevert) =>
    portfolios.updateWith(address) { prev =>
      prev.map(xs => (xs |-| toRevert).filter(_._2 < 0)).filter(_.nonEmpty)
    }
  }

}
