package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import scala.concurrent.{ExecutionContext, Future}

class FeaturesCache(loader: Short => Future[Boolean], expiration: Duration)(implicit executionContext: ExecutionContext)
    extends BlockchainCache[java.lang.Short, Future[java.lang.Boolean]](
      loader = (featureId: java.lang.Short) => loader(featureId) map boolean2Boolean,
      expiration = Some(expiration)
    )
