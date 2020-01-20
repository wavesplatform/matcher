package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import scala.concurrent.{ExecutionContext, Future}

class AssetDescriptionsCache(loader: IssuedAsset => Future[Option[BriefAssetDescription]], expiration: Duration)(
    implicit executionContext: ExecutionContext)
    extends BlockchainCache[IssuedAsset, Option[BriefAssetDescription]](loader,
                                                                        Some(expiration),
                                                                        invalidationPredicate = BlockchainCache.noCustomInvalidationLogic)
