package com.wavesplatform.dex.settings.utils

object validationOf {
  def apply[A, F]: ConfigValidate[A, F] = new ConfigValidate[A, F]
}
