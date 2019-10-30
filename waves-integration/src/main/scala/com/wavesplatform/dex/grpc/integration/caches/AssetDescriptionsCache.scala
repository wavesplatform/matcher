package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.concurrent.{ExecutionContext, Future}

class AssetDescriptionsCache(loader: IssuedAsset => Future[Option[BriefAssetDescription]], expiration: Duration)(
    implicit executionContext: ExecutionContext)
    extends BlockchainCache[IssuedAsset, Future[Option[BriefAssetDescription]]](loader, Some(expiration))
