package com.wavesplatform.dex.it.api.dex

import cats.Functor
import cats.syntax.functor._
import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.fp.{CanRepeat, RepeatRequestOptions}
import im.mak.waves.transactions.ExchangeTransaction

import scala.concurrent.duration.DurationInt

object DexApiSyntax {

  implicit final class Ops[F[_]: Functor](val self: DexApi[F])(implicit R: CanRepeat[F]) {

    def tradingPairInfo(assetPair: AssetPair): F[Option[HttpMarketDataWithMeta]] = self.getOrderBooks.map {
      _.markets.find(marketData => marketData.amountAsset == assetPair.amountAsset && marketData.priceAsset == assetPair.priceAsset)
    }

    def waitForOrderStatus(order: Order, status: HttpOrderStatus.Status): F[HttpOrderStatus] =
      waitForOrderStatus(order.assetPair, order.id(), status)

    def waitForOrderStatus(assetPair: AssetPair, id: Order.Id, status: HttpOrderStatus.Status): F[HttpOrderStatus] =
      waitForOrder(assetPair, id)(_.status == status)

    def waitForOrder(order: Order)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus] =
      waitForOrder(order.assetPair, order.id())(pred)

    def waitForOrder(assetPair: AssetPair, id: Order.Id)(pred: HttpOrderStatus => Boolean): F[HttpOrderStatus] =
      R.repeatUntil(self.getOrderStatus(assetPair, id), RepeatRequestOptions.default)(pred)

    def waitForOrderPlacement(order: Order): F[HttpSuccessfulPlace] = R.repeatUntil(self.place(order))(_.success)

    def waitForOrderHistory[A](owner: KeyPair, activeOnly: Option[Boolean])(
      pred: List[HttpOrderBookHistoryItem] => Boolean
    ): F[List[HttpOrderBookHistoryItem]] =
      R.repeatUntil(self.getOrderHistoryByPublicKey(owner, activeOnly), RepeatRequestOptions.default)(pred)

    def waitForTransactionsByOrder(order: Order, atLeast: Int): F[List[ExchangeTransaction]] =
      waitForTransactionsByOrder(order.id(), atLeast)

    def waitForTransactionsByOrder(id: Order.Id, atLeast: Int): F[List[ExchangeTransaction]] =
      waitForTransactionsByOrder(id)(_.lengthCompare(atLeast) >= 0)

    def waitForTransactionsByOrder(id: Order.Id)(pred: List[ExchangeTransaction] => Boolean): F[List[ExchangeTransaction]] =
      R.repeatUntil(self.getTransactionsByOrder(id), RepeatRequestOptions.default)(pred)

    def waitForCurrentOffset(pred: Long => Boolean): F[HttpOffset] =
      R.repeatUntil(self.getCurrentOffset, RepeatRequestOptions(1.second, 120))(pred)

    def waitForWsConnections(pred: HttpWebSocketConnections => Boolean): F[HttpWebSocketConnections] =
      R.repeatUntil(self.wsConnections, RepeatRequestOptions(1.second, 120))(pred)

  }

}
