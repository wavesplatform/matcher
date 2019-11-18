package com.wavesplatform.dex.grpc.integration.exceptions

final case class WavesNodeConnectionLostException(message: String, cause: Throwable) extends Exception(message, cause)
