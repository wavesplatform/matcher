package com.wavesplatform.dex.model

import cats.instances.future.catsStdInstancesForFuture
import cats.syntax.either._
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.error
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.metrics.TimerExt
import com.wavesplatform.dex.model.AssetPairBuilder.AssetSide
import com.wavesplatform.dex.settings.MatcherSettings
import kamon.Kamon

import scala.concurrent.ExecutionContext

class AssetPairBuilder(settings: MatcherSettings,
                       assetDescription: IssuedAsset => FutureResult[BriefAssetDescription],
                       blacklistedAssets: Set[IssuedAsset])(implicit ec: ExecutionContext) {

  import com.wavesplatform.dex.model.OrderValidator._

  import Ordered._

  private[this] val indices  = settings.priceAssets.zipWithIndex.toMap
  private[this] val timer    = Kamon.timer("matcher.asset-pair-builder")
  private[this] val create   = timer.withTag("action", "create")
  private[this] val validate = timer.withTag("action", "validate")

  def isCorrectlyOrdered(pair: AssetPair): Boolean = (indices.get(pair.priceAsset), indices.get(pair.amountAsset)) match {
    case (None, None)         => pair.priceAsset.compatId < pair.amountAsset.compatId
    case (Some(_), None)      => true
    case (None, Some(_))      => false
    case (Some(pi), Some(ai)) => pi < ai
  }

  private def isBlacklistedByName(asset: IssuedAsset, desc: BriefAssetDescription): Boolean =
    settings.blacklistedNames.exists(_.findFirstIn(desc.name).nonEmpty)

  def validateAssetId(asset: Asset): FutureResult[Asset] = validateAssetId(asset, AssetSide.Unknown)

  private def validateAssetId(asset: Asset, side: AssetSide): FutureResult[Asset] = {
    asset.fold[FutureResult[Asset]] { liftValueAsync(Waves) } { asset =>
      assetDescription(asset) subflatMap { desc =>
        if (blacklistedAssets.contains(asset) || isBlacklistedByName(asset, desc))
          Left(
            side match {
              case AssetSide.Unknown => error.AssetBlacklisted(asset)
              case AssetSide.Amount  => error.AmountAssetBlacklisted(asset)
              case AssetSide.Price   => error.PriceAssetBlacklisted(asset)
            }
          )
        else Right(asset)
      }
    }
  }

  def validateAssetPair(pair: AssetPair): FutureResult[AssetPair] = validate.measure {
    if (settings.allowedAssetPairs contains pair) liftValueAsync(pair)
    else if (settings.whiteListOnly) liftErrorAsync(error.AssetPairIsDenied(pair))
    else
      for {
        _ <- successAsync.ensure { error.AssetPairSameAssets(pair.amountAsset) }(_ => pair.amountAsset != pair.priceAsset)
        _ <- successAsync.ensure { error.OrderAssetPairReversed(pair) }(_ => isCorrectlyOrdered(pair))
        _ <- validateAssetId(pair.priceAsset, AssetSide.Price)
        _ <- validateAssetId(pair.amountAsset, AssetSide.Amount)
      } yield pair
  }

  def createAssetPair(a1: String, a2: String): FutureResult[AssetPair] = create.measure {
    for {
      a1 <- liftAsync { AssetPair.extractAsset(a1).toEither.leftMap(_ => error.InvalidAsset(a1)) }
      a2 <- liftAsync { AssetPair.extractAsset(a2).toEither.leftMap(_ => error.InvalidAsset(a2)) }
      p  <- validateAssetPair { AssetPair(a1, a2) }
    } yield p
  }
}

object AssetPairBuilder {

  val assetIdOrdering: Ordering[Option[ByteStr]] = Ordering.Option[ByteStr]

  private sealed trait AssetSide

  private object AssetSide {
    case object Unknown extends AssetSide
    case object Amount  extends AssetSide
    case object Price   extends AssetSide
  }
}
