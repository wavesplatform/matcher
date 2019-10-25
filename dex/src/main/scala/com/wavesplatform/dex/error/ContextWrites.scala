package com.wavesplatform.dex.error

import cats.Contravariant
import play.api.libs.json.{JsValue, Writes}

trait ContextWrites[-T] {
  def writes(input: T): JsValue
}

object ContextWrites {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T](implicit w: Writes[T]): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T): JsValue = w.writes(input)
  }

  def contextWrites[T](f: T => JsValue): ContextWrites[T] = new ContextWrites[T] {
    override def writes(input: T): JsValue = f(input)
  }

  implicit val contravariant = new Contravariant[ContextWrites] {
    override def contramap[A, B](fa: ContextWrites[A])(f: B => A): ContextWrites[B] =
      ContextWrites.contextWrites[B](b => fa.writes(f(b)))
  }
}
