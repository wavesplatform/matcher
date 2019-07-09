package com.wavesplatform.dex.error

trait ContextShow[-T] {
  def show(input: T)(context: ErrorFormatterContext): String
}

object ContextShow {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T]: ContextShow[T] = show(_.toString)

  def show[T](f: T => String): ContextShow[T] = contextShow((x, _) => f(x))

  def contextShow[T](f: (T, ErrorFormatterContext) => String): ContextShow[T] = new ContextShow[T] {
    override def show(input: T)(context: ErrorFormatterContext): String = f(input, context)
  }

  implicit final class Ops[A](val self: ContextShow[A]) extends AnyVal {
    def contramap[B](f: B => A): ContextShow[B] = contextShow[B]((b, context) => self.show(f(b))(context))
  }
}
