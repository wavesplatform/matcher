package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

trait AssetsReadOnlyDb[F[_]] {
  def get(asset: IssuedAsset): F[Option[BriefAssetDescription]]
}

object AssetsReadOnlyDb {

  private val cls = "AssetsReadOnlyDb"

  implicit final class AssetsReadOnlyDbOps[F[_]](val self: AssetsReadOnlyDb[F]) extends AnyVal {

    def contains(asset: IssuedAsset)(implicit F: Applicative[F]): F[Boolean] =
      measureDb(cls, "contains") { () =>
        self.get(asset).map(_.nonEmpty)
      }

    def get(asset: Asset)(implicit F: Applicative[F]): F[Option[BriefAssetDescription]] =
      measureDb(cls, "get") { () =>
        asset match {
          case asset: IssuedAsset => self.get(asset)
          case Asset.Waves => BriefAssetDescription.someWavesDescription.pure[F]
        }
      }

    // Can't use MonadError here, because it is not implemented for Id
    def unsafeGet(asset: Asset)(implicit F: Applicative[F]): F[BriefAssetDescription] =
      measureDb(cls, "unsafeGet") { () =>
        get(asset).map(_.getOrElse(throw new RuntimeException(s"Unknown asset: ${asset.toString}")))
      }

    // don't measure them because they use unsafeGet
    def unsafeGetDecimals(asset: Asset)(implicit F: Applicative[F]): F[Int] = unsafeGet(asset).map(_.decimals)
    def unsafeGetHasScript(asset: Asset)(implicit F: Applicative[F]): F[Boolean] = unsafeGet(asset).map(_.hasScript)
  }

}
