package com.wavesplatform.dex.grpc.integration.caches

import scala.concurrent.{ExecutionContext, Future}

class FeaturesCache(loader: Short => Future[Boolean], invalidationPredicate: Boolean => Boolean)(implicit executionContext: ExecutionContext)
    extends BlockchainCache[java.lang.Short, java.lang.Boolean](
      loader = featureId => loader(featureId) map boolean2Boolean,
      expiration = None,
      invalidationPredicate = isFeatureActivated => invalidationPredicate(isFeatureActivated)
    )
