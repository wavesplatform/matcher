package com.wavesplatform.dex.error

import cats.Contravariant

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

  implicit val contravariant = new Contravariant[ContextShow] {
    override def contramap[A, B](fa: ContextShow[A])(f: B => A): ContextShow[B] = ContextShow.contextShow[B]((b, context) => fa.show(f(b))(context))
  }
}
