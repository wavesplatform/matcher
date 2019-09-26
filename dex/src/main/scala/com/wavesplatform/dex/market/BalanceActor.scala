package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef}
import akka.pattern.pipe
import cats.instances.long.catsKernelStdGroupForLong
import cats.instances.map._
import cats.syntax.group._
import com.wavesplatform.account.Address
import com.wavesplatform.dex.market.BalanceActor._
import com.wavesplatform.transaction.Asset

import scala.concurrent.Future

class BalanceActor(spendableBalance: (Address, Asset) => Future[Long]) extends Actor {
  import context.dispatcher

  override def receive: Receive = state(Map.empty)

  private def state(origOpenVolume: Map[Address, Map[Asset, Long]]): Receive = {
    case Command.Reserve(client, xs) =>
      val updatedOpenVolume = origOpenVolume.updated(client, origOpenVolume.getOrElse(client, Map.empty) |+| xs)
      context.become(state(updatedOpenVolume))

    case Command.Release(client, xs) =>
      val updatedOpenVolume = origOpenVolume.updated(client, origOpenVolume.getOrElse(client, Map.empty) |-| xs)
      context.become(state(updatedOpenVolume))

    case Query.GetReservedBalance(requestId, client, forAssets) =>
      sender ! Reply.ReservedBalance(requestId, client, origOpenVolume.getOrElse(client, Map.empty).filter { case (k, _) => forAssets.contains(k) })

    case Query.GetTradableBalance(requestId, client, forAssets) =>
      val s = sender()
      Future
        .traverse(forAssets) { asset =>
          spendableBalance(client, asset).map(v => (asset, v))
        }
        .map { xs =>
          TradableBalanceDraftReply(requestId, client, xs.toMap, s)
        }
        .pipeTo(self)

    case TradableBalanceDraftReply(requestId, client, blockchainBalance, s) =>
      s ! Reply.TradableBalance(requestId, client, blockchainBalance |-| origOpenVolume.getOrElse(client, Map.empty))
  }
}

object BalanceActor {
  sealed trait Query
  object Query {
    case class GetReservedBalance(requestId: Long, client: Address, forAssets: Set[Asset]) extends Query
    case class GetTradableBalance(requestId: Long, client: Address, forAssets: Set[Asset]) extends Query
  }

  sealed trait Reply
  object Reply {
    case class ReservedBalance(requestId: Long, client: Address, balance: Map[Asset, Long]) extends Reply
    case class TradableBalance(requestId: Long, client: Address, balance: Map[Asset, Long]) extends Reply
  }

  sealed trait Command
  object Command {
    case class Reserve(client: Address, assets: Map[Asset, Long]) extends Command {
      require(assets.values.forall(_ > 0))
    }

    case class Release(client: Address, assets: Map[Asset, Long]) extends Command {
      require(assets.values.forall(_ > 0))
    }
  }

  private case class TradableBalanceDraftReply(requestId: Long, client: Address, balance: Map[Asset, Long], recipient: ActorRef)
}
