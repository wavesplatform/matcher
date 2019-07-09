package com.wavesplatform.dex.error

import play.api.libs.json.{JsValue, Writes}

trait ContextWrites[-T] {
  def writes(input: T)(context: ErrorFormatterContext): JsValue
}

object ContextWrites {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T](implicit w: Writes[T]): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T)(context: ErrorFormatterContext): JsValue = w.writes(input)
  }

  def writes[T](f: (T, ErrorFormatterContext) => JsValue): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T)(context: ErrorFormatterContext): JsValue = f(input, context)
  }

  implicit final class Ops[A](val self: ContextWrites[A]) extends AnyVal {
    def contramap[B](f: B => A): ContextWrites[B] = ContextWrites.writes[B]((b, context) => self.writes(f(b))(context))
  }
}
