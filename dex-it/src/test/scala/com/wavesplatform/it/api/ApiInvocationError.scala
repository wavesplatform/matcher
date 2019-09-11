package com.wavesplatform.it.api

final class ApiInvocationError(cause: MatcherError) extends RuntimeException(s"$cause")
