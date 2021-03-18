package com.wavesplatform.dex.exceptions

import scala.util.control.NoStackTrace

final case class BinaryMessagesNotSupportedException(message: String) extends Exception(message) with NoStackTrace