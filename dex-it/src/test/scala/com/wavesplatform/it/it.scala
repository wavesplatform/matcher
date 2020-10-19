package com.wavesplatform

import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.waves.WavesFeeConstants._
import com.wavesplatform.it.api.MatcherCommand
import org.scalacheck.Gen

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

package object it {

  /**
   * @return The number of successful commands
   */
  def executeCommands(xs: Seq[MatcherCommand], ignoreErrors: Boolean = true, timeout: FiniteDuration = 3.minutes): Int =
    Await.result(Future.sequence(xs.map(executeCommand(_, ignoreErrors))), timeout).sum

  private def executeCommand(x: MatcherCommand, ignoreErrors: Boolean): Future[Int] =
    try x match {
      case MatcherCommand.Place(dex, order) => dex.asyncTryApi.place(order).map(_.fold(_ => 0, _ => 1))
      case MatcherCommand.Cancel(dex, owner, order) =>
        dex.asyncTryApi.cancel(owner, order).map(_.fold(_ => 0, _ => 1))
    } catch {
      case NonFatal(e) =>
        if (ignoreErrors) Future.successful(0)
        else Future.failed(e)
    }

  def orderGen(
    matcher: PublicKey,
    trader: KeyPair,
    assetPairs: Seq[AssetPair],
    types: Seq[OrderType] = Seq(OrderType.BUY, OrderType.SELL)
  ): Gen[Order] = {
    val ts = System.currentTimeMillis()
    for {
      assetPair <- Gen.oneOf(assetPairs)
      tpe <- Gen.oneOf(types)
      amount <- Gen.choose(10, 100)
      price <- Gen.choose(10, 100)
      orderVersion <- Gen.choose[Byte](1, 3)
      expirationDiff <- Gen.choose(600000, 6000000)
    } yield
      if (tpe == OrderType.BUY)
        Order.buy(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          ts,
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
      else
        Order.sell(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          ts,
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
  }

  def choose[T](xs: IndexedSeq[T]): T = xs(Random.nextInt(xs.size))
}
