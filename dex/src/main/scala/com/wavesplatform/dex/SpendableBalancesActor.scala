package com.wavesplatform.dex

import akka.actor.{Actor, ActorRef}
import akka.pattern.pipe
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group.catsSyntaxGroup
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup

import scala.concurrent.Future

class SpendableBalancesActor(spendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]],
                             allAssetsSpendableBalances: Address => Future[Map[Asset, Long]],
                             addressDirectory: ActorRef)
    extends Actor {

  import context.dispatcher

  /** Keeps states of all addresses by all available assets */
  var fullState: Map[Address, Map[Asset, Long]] = Map.empty

  /** Keeps balance changes of addresses for which there is no full state yet (addresses that were not requested for snapshot) */
  var incompleteStateChanges: Map[Address, Map[Asset, Long]] = Map.empty

  def receive: Receive = {

    case SpendableBalancesActor.Query.GetState(address, assets) =>
      val maybeAddressState            = fullState.get(address).orElse(incompleteStateChanges get address)
      val assetsMaybeBalances          = assets.map(asset => asset -> maybeAddressState.flatMap(_ get asset)).toMap
      val (knownAssets, unknownAssets) = assetsMaybeBalances.partition { case (_, balance) => balance.isDefined }

      lazy val knownPreparedState = knownAssets.collect { case (a, Some(b)) => a -> b }

      if (unknownAssets.isEmpty) sender ! SpendableBalancesActor.Reply.GetState(knownPreparedState)
      else
        spendableBalances(address, unknownAssets.keySet) // TODO what if this future fail?
          .map(stateFromNode => SpendableBalancesActor.NodeBalanceRequestRoundtrip(address, knownAssets.keySet, stateFromNode))
          .pipeTo(self)(sender)

    case SpendableBalancesActor.NodeBalanceRequestRoundtrip(address, knownAssets, stateFromNode) =>
      if (!fullState.contains(address)) {
        incompleteStateChanges = incompleteStateChanges.updated(address, stateFromNode ++ incompleteStateChanges.getOrElse(address, Map.empty))
      }

      val assets = stateFromNode.keySet ++ knownAssets
      val result = fullState.getOrElse(address, incompleteStateChanges(address)).filterKeys(assets)

      sender ! SpendableBalancesActor.Reply.GetState(result)

    case SpendableBalancesActor.Query.GetSnapshot(address) =>
      fullState.get(address) match {
        case Some(state) => sender ! SpendableBalancesActor.Reply.GetSnapshot(state)
        case None        => allAssetsSpendableBalances(address).map(SpendableBalancesActor.Command.SetState(address, _)).pipeTo(self)(sender)
      }

    case SpendableBalancesActor.Command.SetState(address, state) =>
      val addressState = state ++ incompleteStateChanges.getOrElse(address, Map.empty)
      fullState += address -> addressState
      incompleteStateChanges -= address
      sender ! SpendableBalancesActor.Reply.GetSnapshot(addressState)

    case SpendableBalancesActor.Command.UpdateStates(changes) =>
      changes.foreach {
        case (address, stateUpdate) =>
          fullState.get(address) match {
            case Some(addressFullState) =>
              val cleanStateUpdate = stateUpdate.filter { case (a, updatedBalance) => addressFullState.get(a).fold(true)(updatedBalance != _) }
              addressDirectory ! AddressDirectory.Envelope(address, AddressActor.Command.CancelNotEnoughCoinsOrders(cleanStateUpdate))
              fullState = fullState.updated(address, addressFullState ++ cleanStateUpdate)
            case None =>
              addressDirectory ! AddressDirectory.Envelope(address, AddressActor.Command.CancelNotEnoughCoinsOrders(stateUpdate))
              incompleteStateChanges = incompleteStateChanges.updated(address, incompleteStateChanges.getOrElse(address, Map.empty) ++ stateUpdate)
          }
      }

    // Subtract is called when there is a web socket connection and thus we have `fullState` for this address
    case SpendableBalancesActor.Command.Subtract(address, balance) => fullState = fullState.updated(address, fullState(address) |-| balance)
  }
}

object SpendableBalancesActor {

  trait Command
  object Command {
    final case class SetState(address: Address, state: Map[Asset, Long])   extends Command
    final case class Subtract(address: Address, balance: Map[Asset, Long]) extends Command
    final case class UpdateStates(changes: Map[Address, Map[Asset, Long]]) extends Command
  }

  trait Query
  object Query {
    final case class GetState(address: Address, assets: Set[Asset]) extends Query
    final case class GetSnapshot(address: Address)                  extends Query
  }

  trait Reply
  object Reply {
    final case class GetState(state: Map[Asset, Long])    extends Reply
    final case class GetSnapshot(state: Map[Asset, Long]) extends Reply
  }

  final case class NodeBalanceRequestRoundtrip(address: Address, knownAssets: Set[Asset], stateFromNode: Map[Asset, Long])
}
