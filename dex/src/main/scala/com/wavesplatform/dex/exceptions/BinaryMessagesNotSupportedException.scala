package com.wavesplatform.dex.exceptions

import scala.util.control.NoStackTrace

final case class BinaryMessagesNotSupportedException(message: String, cause: Throwable) extends Exception(message, cause) with NoStackTrace