package com.wavesplatform.dex.error

import cats.Contravariant
import play.api.libs.json.{JsValue, Writes}

trait ContextWrites[-T] {
  def writes(input: T)(context: ErrorFormatterContext): JsValue
}

object ContextWrites {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T](implicit w: Writes[T]): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T)(context: ErrorFormatterContext): JsValue = w.writes(input)
  }

  def contextWrites[T](f: (T, ErrorFormatterContext) => JsValue): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T)(context: ErrorFormatterContext): JsValue = f(input, context)
  }

  implicit val contravariant = new Contravariant[ContextWrites] {
    override def contramap[A, B](fa: ContextWrites[A])(f: B => A): ContextWrites[B] =
      ContextWrites.contextWrites[B]((b, context) => fa.writes(f(b))(context))
  }
}
