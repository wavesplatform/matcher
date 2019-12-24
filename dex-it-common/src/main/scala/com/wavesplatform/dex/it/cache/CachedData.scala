package com.wavesplatform.dex.it.cache

import java.util.concurrent.locks.ReentrantReadWriteLock

class CachedData[T <: AnyRef](getData: => T) {

  private val lock  = new ReentrantReadWriteLock()
  private val read  = lock.readLock()
  private val write = lock.writeLock()

  private var cached = Option.empty[T]

  def get(): T =
    try {
      read.lock()
      cached match {
        case Some(x) => x
        case None =>
          val r = getData
          cached = Some(r)
          r
      }
    } finally read.unlock()

  def invalidate(): Unit =
    try {
      write.lock()
      cached = None
    } finally write.unlock()
}

object CachedData {
  def apply[T <: AnyRef](getData: => T): CachedData[T] = new CachedData[T](getData)
}
