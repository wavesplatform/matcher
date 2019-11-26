package com.wavesplatform.dex.error

import cats.Contravariant

// TODO: Replace by Show?
trait ContextShow[-T] {
  def show(input: T): String
}

object ContextShow {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T]: ContextShow[T] = show(_.toString)

  def show[T](f: T => String): ContextShow[T] = contextShow(f)

  def contextShow[T](f: T => String): ContextShow[T] = new ContextShow[T] {
    override def show(input: T): String = f(input)
  }

  implicit val contravariant = new Contravariant[ContextShow] {
    override def contramap[A, B](fa: ContextShow[A])(f: B => A): ContextShow[B] = ContextShow.contextShow[B](b => fa.show(f(b)))
  }
}
