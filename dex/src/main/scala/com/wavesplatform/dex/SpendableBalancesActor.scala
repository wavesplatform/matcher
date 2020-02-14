package com.wavesplatform.dex

import akka.actor.Actor
import akka.pattern.pipe
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group.catsSyntaxGroup
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup

import scala.concurrent.{ExecutionContext, Future}

class SpendableBalancesActor(allAssetsSpendableBalances: Address => Future[Map[Asset, Long]])(implicit ec: ExecutionContext) extends Actor {

  var fullState: Map[Address, Map[Asset, Long]]  = Map.empty
  var addressDff: Map[Address, Map[Asset, Long]] = Map.empty

  def receive: Receive = {

    case SpendableBalancesActor.Query.GetSnapshot(address) =>
      fullState.get(address) match {
        case Some(state) => sender ! SpendableBalancesActor.Reply.GetSnapshot(state)
        case None        => allAssetsSpendableBalances(address).map(SpendableBalancesActor.Command.SetState(address, _)).pipeTo(self)(sender)
      }

    case SpendableBalancesActor.Command.SetState(address, state) =>
      val addressState = state ++ addressDff.getOrElse(address, Map.empty)
      fullState += address -> addressState
      addressDff -= address
      sender ! SpendableBalancesActor.Reply.GetSnapshot(addressState)

    case SpendableBalancesActor.Command.UpdateDiff(changes) =>
      changes.foreach {
        case (address, diff) =>
          fullState.get(address) match {
            case Some(addressFullState) => fullState = fullState.updated(address, addressFullState ++ diff)
            case None                   => addressDff = addressDff.updated(address, addressDff.getOrElse(address, Map.empty) ++ diff)
          }
      }

    case SpendableBalancesActor.Command.Subtract(address, balance) =>
      fullState = fullState.updated(address, fullState(address) |-| balance)

    case SpendableBalancesActor.Query.GetState(address, assets) =>
      sender ! SpendableBalancesActor.Reply.GetState(fullState getOrElse (address, Map.empty) filterKeys assets.contains)
  }
}

object SpendableBalancesActor {

  trait Command
  object Command {
    final case class SetState(address: Address, state: Map[Asset, Long])      extends Command
    final case class Subtract(address: Address, balance: Map[Asset, Long])    extends Command
    final case class UpdateDiff(updatedState: Map[Address, Map[Asset, Long]]) extends Command
  }

  trait Query
  object Query {
    final case class GetSnapshot(address: Address)                  extends Query
    final case class GetState(address: Address, assets: Set[Asset]) extends Query
  }

  trait Reply
  object Reply {
    final case class GetSnapshot(state: Map[Asset, Long]) extends Reply
    final case class GetState(state: Map[Asset, Long])    extends Reply
  }

}
