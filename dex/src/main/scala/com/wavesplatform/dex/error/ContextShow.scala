package com.wavesplatform.dex.error

trait ContextShow[-T] {
  def show(input: T)(context: ErrorFormatterContext): String
}

object ContextShow {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T]: ContextShow[T] = show(_.toString)

  def show[T](f: T => String): ContextShow[T] = new ContextShow[T] {
    override def show(input: T)(context: ErrorFormatterContext): String = f(input)
  }
}
