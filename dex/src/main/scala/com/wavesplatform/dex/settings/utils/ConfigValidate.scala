package com.wavesplatform.dex.settings.utils

import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailure
import shapeless.{HList, LabelledGeneric}
import shapeless.ops.hlist.Selector
import shapeless.ops.record.Keys
import shapeless.tag.Tagged

class ConfigValidate[ObjectT, FieldName] {
  def mk[L <: HList, KeyList <: HList](f: ObjectT => Option[String])(implicit
      ev: LabelledGeneric.Aux[ObjectT, L],
      ev2: Keys.Aux[L, KeyList],
      ev3: Selector[KeyList, Symbol with Tagged[FieldName]]
  ): (ObjectT, ConfigCursor) => Option[ConfigReaderFailure] = {
    val keys: KeyList = Keys[ev.Repr].apply()
    val fieldName     = keys.select[Symbol with Tagged[FieldName]].name

    (obj: ObjectT, c: ConfigCursor) =>
      c.fluent.at(fieldName).cursor match {
        case Right(c) => f(obj).map(RawFailureReason).map(c.failureFor)
        case Left(_)  => throw new RuntimeException("Imposibru!")
      }
  }
}
