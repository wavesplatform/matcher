package com.wavesplatform.dex.settings.utils

object validationOf {
  def field[A, F]: ConfigFieldValidate[A, F] = new ConfigFieldValidate[A, F]
  def list[L]: ConfigListValidate[L] = new ConfigListValidate[L]
}
