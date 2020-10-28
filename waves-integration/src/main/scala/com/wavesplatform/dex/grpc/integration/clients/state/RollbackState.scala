package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.state.RollbackState._

import scala.annotation.tailrec

case class RollbackState(
  forkHeight: Int,
  accumulatedState: BlockchainState
) {

  def resolve(origState: BlockchainState): Option[ResolveResult] =
    if (accumulatedState.height <= origState.height) None
    else {
      val (fork, common) = origState.changedAddresses.splitOnCondUnordered(_.height >= forkHeight)
      val toInvalidate = fork.view.flatMap(_.addresses).toSet
      val toRequest = toInvalidate -- accumulatedState.changedAddresses.flatMap(_.addresses)

      val updatedState = BlockchainState(
        height = accumulatedState.height,
        changedAddresses = accumulatedState.changedAddresses ::: common,
        regularBalances = replace(origState.regularBalances, accumulatedState.regularBalances),
        outLeases = origState.outLeases ++ accumulatedState.outLeases,
        utxExpenses = replace(origState.utxExpenses, accumulatedState.utxExpenses)
      )

      Some(ResolveResult(updatedState, toRequest))
    }

}

object RollbackState {

  case class ResolveResult(updatedState: BlockchainState, requestInfoFor: Set[Address])

  implicit final class ListOps[T](val self: List[T]) extends AnyVal {

    /**
     * @return (p(x) == true, rest xs), the order is not guaranteed!
     */
    def splitOnCondUnordered(p: T => Boolean): (List[T], List[T]) = {
      @tailrec def loop(accLeft: List[T]): (List[T], List[T]) = self match {
        case Nil => (accLeft, Nil)
        case x :: xs =>
          val updatedAccLeft = x :: accLeft
          if (p(x)) loop(updatedAccLeft)
          else (updatedAccLeft, xs)
      }

      loop(List.empty)
    }

  }

  def replace(orig: Map[Address, Map[Asset, Long]], update: Map[Address, Map[Asset, Long]]): Map[Address, Map[Asset, Long]] =
    update.foldLeft(orig) {
      case (r, (address, xs)) =>
        val orig = r.getOrElse(address, Map.empty)
        r.updated(address, orig ++ xs)
    }

}
