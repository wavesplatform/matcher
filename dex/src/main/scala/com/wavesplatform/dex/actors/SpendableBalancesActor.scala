package com.wavesplatform.dex.actors

import akka.actor.{Actor, ActorRef, Props, Status}
import cats.syntax.either._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.error.{MatcherError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException

import scala.concurrent.Future
import scala.util.{Failure, Success}

class SpendableBalancesActor(
  spendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]],
  allAssetsSpendableBalances: (Address, Set[Asset]) => Future[Map[Asset, Long]], // Set[Asset] to ignore
  addressDirectory: ActorRef
) extends Actor
    with ScorexLogging {

  import context.dispatcher

  def receive: Receive = {

    // Ask client
    case SpendableBalancesActor.Query.GetState(address, assets) =>
      val s = sender()
      spendableBalances(address, assets).onComplete {
        case Success(r) => s ! SpendableBalancesActor.Reply.GetState(r)
        case Failure(ex) =>
          log.error("Could not receive spendable balance from Waves Node", ex)
          s ! Status.Failure(WavesNodeConnectionLostException("Could not receive spendable balance from Waves Node", ex))
      }

    case SpendableBalancesActor.Query.GetSnapshot(address, ignoreAssets) =>
      val s = sender()
      allAssetsSpendableBalances(address, ignoreAssets).onComplete {
        case Success(balance) => s ! SpendableBalancesActor.Reply.GetSnapshot(balance.asRight)
        case Failure(ex) =>
          log.error("Could not receive address spendable balance snapshot from Waves Node", ex)
          s ! SpendableBalancesActor.Reply.GetSnapshot(WavesNodeConnectionBroken.asLeft)
      }

    case SpendableBalancesActor.Command.UpdateStates(changes) =>
      changes.foreach {
        case (address, stateUpdate) =>
          // send only when it become smaller
          addressDirectory ! AddressDirectoryActor.Envelope(address, AddressActor.Message.BalanceChanged(stateUpdate.keySet, stateUpdate))
      }
  }

  /**
   * Splits balance state update into clean (unknown so far) changes and
   * changes for audit (decreasing compared to known balance, they can lead to orders cancelling)
   */
//  private def getCleanAndForAuditChanges(stateUpdate: AddressState, knownBalance: AddressState): (AddressState, AddressState) =
//    stateUpdate.foldLeft((Map.empty[Asset, Long], Map.empty[Asset, Long])) {
//      case ((ac, dc), balanceChanges @ (asset, update)) =>
//        knownBalance.get(asset).fold((ac + balanceChanges, dc + balanceChanges)) { existedBalance =>
//          if (update == existedBalance) (ac, dc) else (ac + balanceChanges, if (update < existedBalance) dc + balanceChanges else dc)
//        }
//    }

}

object SpendableBalancesActor {

  def props(blockchain: WavesBlockchainClient, addressDirectoryRef: ActorRef): Props = Props(
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
    final case class GetSnapshot(address: Address, knownAssets: Set[Asset]) extends Query
  }

  trait Reply

  object Reply {
    final case class GetState(state: Map[Asset, Long]) extends Reply
    final case class GetSnapshot(state: Either[MatcherError, Map[Asset, Long]]) extends Reply
  }

  final case class NodeBalanceRequestRoundtrip(address: Address, knownAssets: Set[Asset], stateFromNode: Map[Asset, Long])
}
