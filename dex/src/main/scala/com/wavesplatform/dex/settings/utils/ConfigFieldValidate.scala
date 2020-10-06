package com.wavesplatform.dex.settings.utils

import pureconfig.ConfigObjectCursor
import pureconfig.error.ConfigReaderFailure
import pureconfig.generic.ProductHint
import shapeless.ops.hlist.Selector
import shapeless.ops.record.Keys
import shapeless.tag.Tagged
import shapeless.{HList, LabelledGeneric}

class ConfigFieldValidate[ObjectT, FieldName] {

  def mk[L <: HList, KeyList <: HList](f: ObjectT => Option[String])(implicit
    ev: LabelledGeneric.Aux[ObjectT, L],
    ev2: Keys.Aux[L, KeyList],
    ev3: Selector[KeyList, Symbol with Tagged[FieldName]]
  ): (ObjectT, ConfigObjectCursor, ProductHint[ObjectT]) => Option[ConfigReaderFailure] = {
    val keys: KeyList = Keys[ev.Repr].apply()
    val fieldName = keys.select[Symbol with Tagged[FieldName]].name

    (obj: ObjectT, c: ConfigObjectCursor, hint: ProductHint[ObjectT]) =>
      f(obj).map(RawFailureReason).map(hint.from(c, fieldName).cursor.failureFor)
  }

}
