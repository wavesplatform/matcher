package com.wavesplatform.dex.settings.utils

import pureconfig.ConfigReader.Result
import pureconfig.error.FailureReason
import pureconfig.{ConfigCursor, ConfigObjectCursor, ConfigReader}

trait ConfigCursorsOps {

  implicit class ConfigObjectCursorOps(private val objectCursor: ConfigObjectCursor) {
    def as[A](path: String)(implicit C: ConfigReader[A]): Result[A] = objectCursor.atKey(path).flatMap(C.from)
  }

  implicit class ConfigCursorOps(private val cursor: ConfigCursor) {
    def as[A](path: String)(implicit C: ConfigReader[A]): Result[A] = cursor.fluent.at(path).cursor.flatMap(C.from)
  }

  implicit def string2FailureReason(s: String): FailureReason = new FailureReason {
    override def description: String = s
  }
}

object ConfigCursorsOps extends ConfigCursorsOps
