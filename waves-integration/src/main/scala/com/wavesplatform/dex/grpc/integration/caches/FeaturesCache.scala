package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}

class FeaturesCache(load: Short => Future[Boolean], expiration: Duration)(implicit executionContext: ExecutionContext)
    extends CacheWithExpiration[Future, java.lang.Short, java.lang.Boolean](
      load = (featureId: java.lang.Short) => load(featureId) map boolean2Boolean,
      expiration = expiration
    )
