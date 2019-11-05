package com.wavesplatform.dex.grpc.integration.clients.async

import cats.syntax.functor._
import cats.{Applicative, Functor}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainAsyncClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import monix.reactive.Observable

object WavesBlockchainAsyncClient {

  type SpendableBalance        = Map[Asset, Long]
  type SpendableBalanceChanges = Map[Address, SpendableBalance]

  final implicit class Ops[F[_]: Functor: Applicative](val self: WavesBlockchainAsyncClient[F]) {

    def assetDescription(asset: Asset): F[Option[BriefAssetDescription]] = {
      asset.fold(Applicative[F].pure { Option(BriefAssetDescription.waves) })(self.assetDescription)
    }

    def assetDecimals(asset: Asset.IssuedAsset): F[Option[Int]] = self.assetDescription(asset).map { _.map(_.decimals) }

    def assetDecimals(asset: Asset): F[Option[Int]] = asset.fold { Applicative[F].pure(Option(8)) } { assetDecimals }
  }
}

trait WavesBlockchainAsyncClient[F[_]] {

  def spendableBalanceChanges: Observable[SpendableBalanceChanges]
  def spendableBalance(address: Address, asset: Asset): F[Long]

  def isFeatureActivated(id: Short): F[Boolean]

  def assetDescription(asset: IssuedAsset): F[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): F[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): F[RunScriptResult]

  def hasScript(address: Address): F[Boolean]
  def runScript(address: Address, input: Order): F[RunScriptResult]

  def wasForged(txIds: Seq[ByteStr]): F[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): F[Boolean]

  def forgedOrder(orderId: ByteStr): F[Boolean]
}
