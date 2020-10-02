package com.wavesplatform.dex.settings.utils

import pureconfig.error.FailureReason

final case class RawFailureReason(description: String) extends FailureReason
