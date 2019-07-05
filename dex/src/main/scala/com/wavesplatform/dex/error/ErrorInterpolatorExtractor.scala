package com.wavesplatform.dex.error

import play.api.libs.json.{JsArray, JsString, JsValue, Writes}

trait ErrorInterpolatorExtractor[-From] {
  def names(input: From): String
  def toString(input: From): String
  def toJson(input: From): JsValue
}

object ErrorInterpolatorExtractor {

  implicit def withSymbol[T](implicit tExtractor: ErrorInterpolatorExtractor[T]): ErrorInterpolatorExtractor[(Symbol, T)] =
    new ErrorInterpolatorExtractor[(Symbol, T)] {
      override def names(input: (Symbol, T)): String    = input._1.name
      override def toString(input: (Symbol, T)): String = tExtractor.toString(input._2)
      override def toJson(input: (Symbol, T)): JsValue  = tExtractor.toJson(input._2)
    }

  def mkJsAuto[T](stringify: T => String)(implicit w: Writes[T]): ErrorInterpolatorExtractor[T] =
    mk(stringify, w.writes)

  def mkJsString[T](stringify: T => String): ErrorInterpolatorExtractor[T] = mk(stringify, x => JsString(stringify(x)))

  def mkCol[C[_], T](f: C[T] => List[T])(implicit itemExtractor: ErrorInterpolatorExtractor[T]): ErrorInterpolatorExtractor[C[T]] =
    new ErrorInterpolatorExtractor[C[T]] {
      override def names(input: C[T]): String    = ""
      override def toString(input: C[T]): String = s"{${f(input).map(itemExtractor.toString).mkString(", ")}}"
      override def toJson(input: C[T]): JsValue  = JsArray(f(input).map(itemExtractor.toJson))
    }

  def mk[T](stringify: T => String, jsonify: T => JsValue): ErrorInterpolatorExtractor[T] =
    new ErrorInterpolatorExtractor[T] {
      override def names(input: T): String    = ""
      override def toString(input: T): String = stringify(input)
      override def toJson(input: T): JsValue  = jsonify(input)
    }
}
