package com.wavesplatform.dex.redis

import kamon.instrumentation.executor.ExecutorInstrumentation
import org.redisson.api.{RStream, StreamMessageId}

import java.util
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

final class RedisStream[K, V](innerStream: RStream[K, V]) {

  private val stex =
    ExecutionContext.fromExecutorService(
      ExecutorInstrumentation.instrument(Executors.newFixedThreadPool(1), "redis-ordered-stream")
    )

  def addOrderedAsync(key: K, value: V): Future[StreamMessageId] =
    Future {
      innerStream.add(key, value)
    }(stex)

  def createReadGroup(groupName: String): Unit = innerStream.createGroup(groupName, StreamMessageId.ALL)
  def removeReadGroup(groupName: String): Unit = innerStream.removeGroup(groupName)

  def read(groupName: String): util.Map[StreamMessageId, util.Map[K, V]] =
    innerStream.readGroup(groupName, s"consumer-${System.currentTimeMillis()}")

}
