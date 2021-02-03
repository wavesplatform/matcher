package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Monoid
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup
import com.wavesplatform.dex.grpc.integration.clients.domain.TransactionWithChanges
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.{ExchangeTransactionData, Transaction}

import java.math.{BigDecimal, RoundingMode}

object Implicits {

  implicit final class UtxTransactionOps(val self: UtxTransaction) extends AnyVal {

    def pessimisticPortfolios: AddressAssets = self.diff.flatMap(_.stateUpdate).fold(Map.empty: AddressAssets) {
      _.pessimisticPortfolios(self.transaction.flatMap(_.transaction))
    }

  }

  implicit final class TransactionWithChangesOps(val self: TransactionWithChanges) extends AnyVal {
    def pessimisticPortfolios: AddressAssets = self.changes.pessimisticPortfolios(self.tx.transaction)
  }

  // Probably we need to move ByteString -> Address conversion further
  implicit final class StateUpdateOps(val self: StateUpdate) extends AnyVal {

    def pessimisticPortfolios(tx: Option[Transaction]): AddressAssets = tx.flatMap(_.data.exchange) match {
      case Some(x) => exchangeTransactionPessimisticPortfolios(x)
      case _ => genericPessimisticPortfolios
    }

    // TODO DEX-1023 Could we do it faster?
    private def genericPessimisticPortfolios: AddressAssets = {
      // Balances
      val p1 = self.balances.groupBy(_.address).flatMap {
        case (address, updates) =>
          val balances = updates.view
            .flatMap(_.amount)
            .collect {
              case x if x.amount < 0 => x.assetId.toVanillaAsset -> x.amount // Count only pessimistic
            }
            .toList
            .foldMap(Map(_))

          if (balances.isEmpty) none
          else (address -> balances).some
      }

      // Leasing
      Monoid
        .combineAll(
          p1 ::
          self.leases.view
            .collect { case x if x.out > 0 => Map(x.address -> Map[Asset, Long](Waves -> -x.out)) }
            .toList
        )
        .map { case (address, xs) => address.toVanillaAddress -> xs }
    }

  }

  def exchangeTransactionPessimisticPortfolios(tx: ExchangeTransactionData): AddressAssets = tx.orders.toList.foldMap { o =>
    val sender = o.senderPublicKey.toVanillaPublicKey.toAddress

    val feeSpending = o.matcherFee.fold(Map.empty[Asset, Long]) { x =>
      val amount = if (o.orderSide.isSell) tx.sellMatcherFee else tx.buyMatcherFee
      if (amount == 0) Map.empty
      else Map(x.assetId.toVanillaAsset -> -amount)
    }

    val assetSpending =
      if (o.orderSide.isSell) Map(o.getAssetPair.amountAssetId.toVanillaAsset -> -tx.amount)
      else {
        val amount = -new BigDecimal(tx.price)
          .multiply(new BigDecimal(tx.amount))
          .scaleByPowerOfTen(-Order.PriceConstantExponent)
          .setScale(0, RoundingMode.FLOOR)
          .longValue()
        Map(o.getAssetPair.priceAssetId.toVanillaAsset -> amount)
      }

    Map(sender -> (assetSpending |+| feeSpending))
  }

}
