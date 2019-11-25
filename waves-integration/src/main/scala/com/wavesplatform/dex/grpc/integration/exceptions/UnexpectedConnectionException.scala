package com.wavesplatform.dex.grpc.integration.exceptions

final case class UnexpectedConnectionException(message: String, cause: Throwable) extends Exception(message, cause)
