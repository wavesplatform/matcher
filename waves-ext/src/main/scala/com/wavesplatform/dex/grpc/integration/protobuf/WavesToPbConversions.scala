package com.wavesplatform.dex.grpc.integration.protobuf

import cats.syntax.option._
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.collections.Implicits.ListOps
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.order.{AssetPair, Order}
import com.wavesplatform.protobuf.transaction.{ExchangeTransactionData, PBTransactions}
import com.wavesplatform.state.{Diff, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{exchange => ve}
import com.wavesplatform.{account => va}

object WavesToPbConversions {

  val pbWaves = Waves.toPB

  implicit final class VanillaExchangeTransactionOps(val tx: ve.ExchangeTransaction) extends AnyVal {

    def toPB: SignedExchangeTransaction =
      SignedExchangeTransaction(
        transaction = Some(
          ExchangeTransaction(
            chainId = tx.chainId.toInt,
            senderPublicKey = tx.sender.toPB,
            fee = Some(Amount(assetId = tx.assetFee._1.toPB, amount = tx.assetFee._2)),
            timestamp = tx.timestamp,
            version = tx.version,
            data = ExchangeTransaction.Data.Exchange(
              ExchangeTransactionData(
                amount = tx.amount,
                price = tx.price,
                buyMatcherFee = tx.buyMatcherFee,
                sellMatcherFee = tx.sellMatcherFee,
                orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
              )
            )
          )
        ),
        proofs = tx.proofs.proofs.map(_.toPB)
      )

  }

  implicit final class VanillaAssetOps(val self: Asset) extends AnyVal {

    def toPB: ByteString = self match {
      case Asset.IssuedAsset(assetId) => assetId.toPB
      case Asset.Waves => ByteString.EMPTY
    }

  }

  implicit final class VanillaAddressOps(val self: Address) extends AnyVal {
    def toPB: ByteString = ByteStr(self.bytes).toPB
  }

  implicit final class VanillaOrderOps(val order: ve.Order) extends AnyVal {

    def toPB: Order =
      Order(
        chainId = va.AddressScheme.current.chainId.toInt,
        senderPublicKey = order.senderPublicKey.toPB,
        matcherPublicKey = order.matcherPublicKey.toPB,
        assetPair = Some(AssetPair(order.assetPair.amountAsset.toPB, order.assetPair.priceAsset.toPB)),
        orderSide = order.orderType match {
          case ve.OrderType.BUY => Order.Side.BUY
          case ve.OrderType.SELL => Order.Side.SELL
        },
        amount = order.amount,
        price = order.price,
        timestamp = order.timestamp,
        expiration = order.expiration,
        matcherFee = Some(Amount(order.matcherFeeAssetId.toPB, order.matcherFee)),
        version = order.version,
        proofs = order.proofs.map(_.toPB)
      )

  }

  implicit final class VanillaDiffOps(val self: Diff) extends AnyVal {

    def toPB: TransactionDiff = {
      val portfolioUpdates = self.portfolios.foldLeft(PortfolioUpdates(Nil, Nil)) {
        case (r, (address, portfolio)) => append(r, address, portfolio)
      }

      TransactionDiff(
        stateUpdate = StateUpdate(
          balances = portfolioUpdates.balanceUpdates,
          leasingForAddress = portfolioUpdates.leasingUpdates,
          dataEntries = self.accountData.view.flatMap {
            case (address, dataEntries) =>
              dataEntries.data.values.map { dataEntry =>
                StateUpdate.DataEntryUpdate(
                  address = address.toPB,
                  dataEntry = PBTransactions.toPBDataEntry(dataEntry).some
                )
              }
          }.toList,
          assets = Nil // Haven't support yet
        ).some
        // Not useful for UTX transactions. Could be useful in the future
        // orderFills = self.orderFills.map { case (orderId, x) => TransactionDiff.OrderFill(orderId.toPB, volume = x.volume, fee = x.fee) }.toSeq
      )
    }

    private def append(init: PortfolioUpdates, address: Address, portfolio: Portfolio): PortfolioUpdates = {
      val pbAddress = address.toPB

      val balanceUpdates = portfolio.assets
        .foldLeft(init.balanceUpdates) {
          case (r, (asset, v)) =>
            if (v == 0) r
            else StateUpdate.BalanceUpdate(
              address = address.toPB,
              amountAfter = Amount(asset.toPB, v).some
            ) :: r
        }
        .prependIf(portfolio.balance != 0)(StateUpdate.BalanceUpdate(pbAddress, Amount(pbWaves, portfolio.balance).some))

      val lease = portfolio.lease
      val leasingUpdates = init.leasingUpdates.prependIf(lease.in != 0 || lease.out != 0) {
        StateUpdate.LeasingUpdate(
          address = pbAddress,
          inAfter = portfolio.lease.in,
          outAfter = portfolio.lease.out
        )
      }

      PortfolioUpdates(balanceUpdates, leasingUpdates)
    }

  }

  implicit final class VanillaByteStrOps(val self: ByteStr) extends AnyVal {
    def toPB: ByteString = ByteString.copyFrom(self.arr)
  }

  private case class PortfolioUpdates(
    balanceUpdates: List[StateUpdate.BalanceUpdate],
    leasingUpdates: List[StateUpdate.LeasingUpdate]
  )

}
