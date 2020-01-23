package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.lang.ValidationError

package object protobuf {

  implicit class EitherVEExt[T](e: Either[ValidationError, T]) {
    def explicitGetErr(): T = e.fold(e => throw GRPCErrors.toStatusException(e), identity)
  }
}
