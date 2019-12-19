package com.wavesplatform.dex.it.api.node

import cats.{Functor, Id}
import com.wavesplatform.dex.it.fp.CanExtract

trait HasWavesNode {

  protected implicit def toNodeExplicitGetOps[F[_]: Functor: CanExtract](self: NodeApi[F]): NodeApiOps.ExplicitGetNodeApiOps[F] = {
    new NodeApiOps.ExplicitGetNodeApiOps[F](self)
  }

  protected def wavesNode1Api: NodeApi[Id]
}
