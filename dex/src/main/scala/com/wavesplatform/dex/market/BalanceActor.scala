package com.wavesplatform.dex.market

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.pipe
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group._
import com.wavesplatform.account.Address
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.market.BalanceActor._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging

import scala.concurrent.Future

class BalanceActor(spendableBalance: (Address, Asset) => Future[Long]) extends Actor with ScorexLogging {
  import context.dispatcher

  override def receive: Receive = state(Map.empty)

  private def state(origOpenVolume: Map[Address, Map[Asset, Long]]): Receive = {
    case Command.AppendReservedBalance(client, xs) =>
      val origClientOpenVolume = origOpenVolume.getOrElse(client, Map.empty)
      val updatedClientOpenVolume = origClientOpenVolume |+| xs
      val updatedOpenVolume = origOpenVolume.updated(client, updatedClientOpenVolume)
      log.trace(s"$client: $origClientOpenVolume -> $updatedClientOpenVolume")
      context.become(state(updatedOpenVolume))

    case Query.GetReservedBalance(requestId, client) =>
      sender ! Reply.ReservedBalance(requestId, origOpenVolume.getOrElse(client, Map.empty))

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
      s ! Reply.TradableBalance(requestId, blockchainBalance |-| origOpenVolume.getOrElse(client, Map.empty))
  }
}

object BalanceActor {
  def props(spendableBalance: (Address, Asset) => Future[Long]) = Props(new BalanceActor(spendableBalance))

  sealed trait Query
  object Query {
    case class GetReservedBalance(requestId: Long, client: Address)                        extends Query
    case class GetTradableBalance(requestId: Long, client: Address, forAssets: Set[Asset]) extends Query
  }

  sealed trait Reply
  object Reply {
    case class ReservedBalance(requestId: Long, balance: Map[Asset, Long]) extends Reply
    case class TradableBalance(requestId: Long, balance: Map[Asset, Long]) extends Reply
  }

  sealed trait Command
  object Command {
    case class AppendReservedBalance(client: Address, assets: Map[Asset, Long]) extends Command
  }

  private case class TradableBalanceDraftReply(requestId: Long, client: Address, balance: Map[Asset, Long], recipient: ActorRef)
}
