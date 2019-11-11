package com.wavesplatform.dex.settings

import java.util.Properties

import com.typesafe.config.Config

final class ConfigExtraOps(val self: Config) extends AnyVal {
  def toProperties: Properties = {
    val r = new Properties()
    self.entrySet().forEach { entry =>
      r.setProperty(entry.getKey, self.getString(entry.getKey))
    }
    r
  }
}
