package com.wavesplatform.dex.exceptions

import scala.util.control.NoStackTrace

final case class BinaryMessagesNotSupportedException(message: String = "Binary messages are not supported") extends Exception(message) with NoStackTrace
