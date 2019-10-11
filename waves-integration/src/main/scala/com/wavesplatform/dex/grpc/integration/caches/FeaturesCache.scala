package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import scala.concurrent.{ExecutionContext, Future}

class FeaturesCache(loader: Short => Future[Boolean], expiration: Duration)(implicit executionContext: ExecutionContext)
    extends BlockchainCache[Future, java.lang.Short, java.lang.Boolean](
      loader = (featureId: java.lang.Short) => loader(featureId) map boolean2Boolean,
      expiration = Some(expiration)
    )
