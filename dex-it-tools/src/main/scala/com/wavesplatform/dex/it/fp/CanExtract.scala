package com.wavesplatform.dex.it.fp

trait CanExtract[F[_]] {
  def extract[ErrorT, ResultT](f: => F[Either[ErrorT, ResultT]]): F[ResultT]
}

object CanExtract extends CanExtractInstances
