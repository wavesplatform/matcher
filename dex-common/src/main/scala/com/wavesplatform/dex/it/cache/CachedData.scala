package com.wavesplatform.dex.it.cache

class CachedData[T <: AnyRef](getData: => T) {

  @volatile private var cached = Option.empty[T]

  def get(): T = synchronized {
    cached match {
      case Some(x) => x
      case None =>
        val r = getData
        cached = Some(r)
        r
    }
  }

  def invalidate(): Unit = synchronized {
    cached = None
  }

}

object CachedData {
  def apply[T <: AnyRef](getData: => T): CachedData[T] = new CachedData[T](getData)
}
