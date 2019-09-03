package com.wavesplatform.it.api

trait Logger[F[_]] {
  def error(msg: => String): F[Unit]
  def error(msg: => String, cause: Throwable): F[Unit]
  def warn(msg: => String): F[Unit]
  def info(msg: => String): F[Unit]
  def debug(msg: => String): F[Unit]
  def trace(msg: => String): F[Unit]
}
