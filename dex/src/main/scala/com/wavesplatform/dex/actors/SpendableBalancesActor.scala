package com.wavesplatform.dex.actors

import akka.actor.{Actor, ActorRef, Props, Status}
import cats.syntax.either._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.{MatcherError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.model.OrderValidator.AsyncBlockchain

import scala.concurrent.Future
import scala.util.{Failure, Success}

class SpendableBalancesActor(
  spendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]],
  allAssetsSpendableBalances: Address => Future[Map[Asset, Long]],
  addressDirectory: ActorRef
) extends Actor
    with ScorexLogging {

  import context.dispatcher

  type AddressState = Map[Asset, Long]
  type State = Map[Address, AddressState]

  /** Keeps states of all addresses by all available assets */
  var fullState: State = Map.empty

  /** Keeps balance changes of addresses for which there is no full state yet (addresses that were not requested for snapshot) */
  var incompleteStateChanges: State = Map.empty

  def receive: Receive = {

    case SpendableBalancesActor.Query.GetState(address, assets) =>
      val maybeAddressState = fullState.get(address).orElse(incompleteStateChanges get address).getOrElse(Map.empty)
      val assetsMaybeBalances = assets.map(asset => asset -> maybeAddressState.get(asset)).toMap

      val (knownAssets, unknownAssets) = assetsMaybeBalances.partition { case (_, balance) => balance.isDefined }
      if (unknownAssets.isEmpty) {
        val knownPreparedState = knownAssets.collect { case (a, Some(b)) => a -> b }
        sender() ! SpendableBalancesActor.Reply.GetState(knownPreparedState)
      } else {
        val requestSender = sender()
        spendableBalances(address, unknownAssets.keySet).onComplete {
          case Success(r) => self.tell(SpendableBalancesActor.NodeBalanceRequestRoundtrip(address, knownAssets.keySet, r), requestSender)
          case Failure(ex) =>
            log.error("Could not receive spendable balance from Waves Node", ex)
            requestSender ! Status.Failure(WavesNodeConnectionLostException("Could not receive spendable balance from Waves Node", ex))
        }
      }

    case SpendableBalancesActor.NodeBalanceRequestRoundtrip(address, knownAssets, staleStateFromNode) =>
      val assets = staleStateFromNode.keySet ++ knownAssets
      val source = fullState.get(address) match {
        case Some(state) => state
        case None =>
          val updated = staleStateFromNode ++ incompleteStateChanges.getOrElse(address, Map.empty)
          incompleteStateChanges = incompleteStateChanges.updated(address, updated)
          updated
      }

      val result = source.filter { case (asset, _) => assets.contains(asset) }
      sender() ! SpendableBalancesActor.Reply.GetState(result)

    case SpendableBalancesActor.Query.GetSnapshot(address) =>
      val requestSender = sender()
      fullState.get(address) match {
        case Some(state) => requestSender ! SpendableBalancesActor.Reply.GetSnapshot(state.filterNot(_._2 == 0).asRight)
        case None =>
          allAssetsSpendableBalances(address).onComplete {
            case Success(balance) => self.tell(SpendableBalancesActor.Command.SetState(address, balance), requestSender)
            case Failure(ex) =>
              log.error("Could not receive address spendable balance snapshot from Waves Node", ex)
              requestSender ! SpendableBalancesActor.Reply.GetSnapshot(WavesNodeConnectionBroken.asLeft)
          }
      }

    case SpendableBalancesActor.Command.SetState(address, state) =>
      val addressState = fullState.get(address) match {
        case Some(r) => r // Could be with multiple simultaneous connections
        case None =>
          val addressState = state ++ incompleteStateChanges.getOrElse(address, Map.empty)
          fullState += address -> addressState
          log.info(s"[$address] Full state is set up") // We don't log values, because there is too much data from the old accounts
          incompleteStateChanges -= address
          addressState
      }
      sender() ! SpendableBalancesActor.Reply.GetSnapshot(addressState.asRight)

    case SpendableBalancesActor.Command.UpdateStates(changes) =>
      changes.foreach {
        case (address, stateUpdate) =>
          val addressFullState = fullState.get(address)
          val knownBalance = addressFullState orElse incompleteStateChanges.get(address) getOrElse Map.empty
          val (clean, forAudit) = if (knownBalance.isEmpty) (stateUpdate, stateUpdate) else getCleanAndForAuditChanges(stateUpdate, knownBalance)

          if (addressFullState.isDefined) {
            fullState = fullState.updated(address, knownBalance ++ clean)
            log.info(s"[$address] Full state updates: $clean")
          } else incompleteStateChanges = incompleteStateChanges.updated(address, knownBalance ++ clean)

          addressDirectory ! AddressDirectoryActor.Envelope(address, AddressActor.Message.BalanceChanged(clean.keySet, forAudit))
      }
  }

  /**
   * Splits balance state update into clean (unknown so far) changes and
   * changes for audit (decreasing compared to known balance, they can lead to orders cancelling)
   */
  private def getCleanAndForAuditChanges(stateUpdate: AddressState, knownBalance: AddressState): (AddressState, AddressState) =
    stateUpdate.foldLeft((Map.empty[Asset, Long], Map.empty[Asset, Long])) {
      case ((ac, dc), balanceChanges @ (asset, update)) =>
        knownBalance.get(asset).fold((ac + balanceChanges, dc + balanceChanges)) { existedBalance =>
          if (update == existedBalance) (ac, dc) else (ac + balanceChanges, if (update < existedBalance) dc + balanceChanges else dc)
        }
    }

}

object SpendableBalancesActor {

  def props(blockchain: AsyncBlockchain, addressDirectoryRef: ActorRef): Props = Props(
    new SpendableBalancesActor(
      blockchain.spendableBalances,
      blockchain.allAssetsSpendableBalance,
      addressDirectoryRef
    )
  )

  trait Command

  object Command {
    final case class SetState(address: Address, state: Map[Asset, Long]) extends Command
    final case class UpdateStates(changes: Map[Address, Map[Asset, Long]]) extends Command
  }

  trait Query

  object Query {
    final case class GetState(address: Address, assets: Set[Asset]) extends Query
    final case class GetSnapshot(address: Address) extends Query
  }

  trait Reply

  object Reply {
    final case class GetState(state: Map[Asset, Long]) extends Reply
    final case class GetSnapshot(state: Either[MatcherError, Map[Asset, Long]]) extends Reply
  }

  final case class NodeBalanceRequestRoundtrip(address: Address, knownAssets: Set[Asset], stateFromNode: Map[Asset, Long])
}
