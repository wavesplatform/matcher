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
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._

class DefaultPessimisticPortfolios(storage: PessimisticStorage) extends PessimisticPortfolios with ScorexLogging {
  override def getAggregated(address: Address): Map[Asset, Long] = storage.portfolios.getOrElse(address, Map.empty)

  override def replaceWith(setTxs: Seq[PessimisticTransaction]): Set[Address] = {
    val origTxIds = storage.txs.keySet.toSet
    val setTxMap = setTxs.map(x => x.txId -> x.pessimisticPortfolio).toMap
    val setTxIds = setTxMap.keySet

    val putTxIds = setTxIds -- origTxIds
    val removeTxIds = origTxIds -- setTxIds

    if (removeTxIds.isEmpty && putTxIds.isEmpty) Set.empty
    else {
      // It is safe to use setTxMap.apply here, because putTxIds contains elements only from setTxIds
      val putTxsPortfolios = putTxIds.toList.map(id => id -> setTxMap(id))
      val addPortfolios = putTxsPortfolios.foldMap { case (id, p) =>
        storage.txs.put(id, p)
        p
      }

      val subtractPortfolios = removeTxIds.toList.foldMap(storage.txs.remove(_).getOrElse(Map.empty))

      val diff = addPortfolios |-| subtractPortfolios
      diff.foreach { case (address, diff) =>
        storage.portfolios.updateWith(address) { prev =>
          (prev.getOrElse(Map.empty) |+| diff)
            // .filter(_._2 < 0) // Don't need, because transactions of addPortfolios and subtractPortfolios don't interfere
            .some
        }
      }
      log.info(s"replaceWith ids=${setTxMap.keySet.map(_.toVanilla)}, diff=$diff, txs=$setTxs")
      diff.keySet
    }
  }

  override def addPending(txs: Seq[PessimisticTransaction]): Set[Address] = {
    log.info(s"addPending: ${txs.map(_.txId.toVanilla).mkString(", ")}")
    txs.toList.foldMapK[Set, Address](addUnsafe)
  }

  /**
   * @return (affected addresses, unknown transactions)
   */
  override def processForged(txIds: Seq[ByteString]): (Set[Address], List[ByteString]) = {
    log.info(s"processForged: ${txIds.map(_.toVanilla)}")
    txIds.toList.foldMap { txId =>
      val (known, affected) = removeUnsafe(txId)
      (affected, if (known) Nil else List(txId))
    }
  }

  override def removeFailed(txIds: Seq[ByteString]): Set[Address] = ???

  private def addUnsafe(tx: PessimisticTransaction): Set[Address] = {
    val id = tx.txId
    if (storage.txs.contains(id)) {
      log.info(s"addUnsafe: already has ${id.toVanilla}")
      Set.empty
    } else {
      val finalP = tx.pessimisticPortfolio
      log.info(s"addUnsafe: id=${id.toVanilla}, diff=$finalP, tx=$tx")
      // TODO we calculate and check only in the and?
      if (storage.txs.put(id, finalP).isEmpty) {
        finalP.foreach {
          case (address, p) => storage.portfolios.updateWith(address)(_.foldLeft(p)(_ |+| _).some)
        }
        finalP.keySet
      } else Set.empty
    }
  }

  /**
   * @return (known?, affected addresses)
   */
  private def removeUnsafe(txId: ByteString): (Boolean, Set[Address]) = {
    val tx = storage.txs.remove(txId)
    val addectedAddresses = tx match {
      case None =>
        log.info(s"removeUnsafe: wasn't id=${txId.toVanilla}")
        Set.empty[Address]
      case Some(p) =>
        log.info(s"removeUnsafe: id=${txId.toVanilla}, diff=$p")
        p.foreach {
          case (address, p) =>
            storage.portfolios.updateWith(address) { prev =>
              val r = prev.map(_ |-| p)
              log.info(s"removeUnsafe of $address: $prev -> $r")
              r
            }
        }
        p.keySet
    }
    (tx.isDefined, addectedAddresses)
  }

}
