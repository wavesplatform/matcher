package com.wavesplatform.dex.settings.utils

import com.typesafe.config.{ConfigObject, ConfigValue, ConfigValueType}
import pureconfig.ConfigCursor
import pureconfig.ConfigReader.Result
import pureconfig.error.{CollidingKeys, ConfigReaderFailures, WrongType}
import pureconfig.generic.CoproductHint.Use
import pureconfig.generic.error.{CoproductHintException, UnexpectedValueForFieldCoproductHint}
import pureconfig.generic.{CoproductHint, FieldCoproductHint}
import pureconfig.syntax.AnyWriterOps

class WrappedDescendantHint[T](key: String = "type") extends CoproductHint[T] {
  protected def fieldValue(name: String): String = FieldCoproductHint.defaultMapping(name)

  override def from(cursor: ConfigCursor, options: Seq[String]): Result[CoproductHint.Action] =
    for {
      objCur <- cursor.asObjectCursor
      valueCur <- objCur.atKey(key)
      valueStr <- valueCur.asString
      option <-
        options
          .find(valueStr == fieldValue(_))
          .toRight(
            ConfigReaderFailures(valueCur.failureFor(UnexpectedValueForFieldCoproductHint(valueCur.valueOpt.get)))
          )
    } yield Use(objCur.atKeyOrUndefined(valueStr), option)

  // HACK: Probably this wont work. We don't care because don't use this functionality
  override def to(value: ConfigValue, name: String): ConfigValue = value match {
    case co: ConfigObject if co.containsKey(key) => throw CoproductHintException(CollidingKeys(key, co.get(key)))
    case co: ConfigObject => Map(key -> fieldValue(name)).toConfig.withFallback(co.toConfig)
    case _ => throw CoproductHintException(WrongType(value.valueType, Set(ConfigValueType.OBJECT)))
  }

}
