package com.wavesplatform.dex.json

import com.wavesplatform.dex.fp.MayBeEmpty
import play.api.libs.functional.syntax._
import play.api.libs.json._

object Implicits {
  final implicit class JsPathOps(val self: JsPath) extends AnyVal {
    def formatMayBeEmpty[T](implicit f: Format[T], mayBeEmpty: MayBeEmpty[T]): OFormat[T] =
      self
        .formatNullable[T]
        .inmap[T](
          {
            case None    => mayBeEmpty.empty
            case Some(x) => x
          }, { x =>
            if (mayBeEmpty.isEmpty(x)) None else Some(x)
        }
      )
  }
}
