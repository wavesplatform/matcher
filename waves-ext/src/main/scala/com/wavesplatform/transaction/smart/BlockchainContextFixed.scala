package com.wavesplatform.transaction.smart

import cats._
import cats.syntax.semigroup._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state._
import monix.eval.Coeval

import java.util

object BlockchainContextFixed {

  type In = WavesEnvironment.In

  private[this] val cache = new util.HashMap[(StdLibVersion, DirectiveSet), CTX[Environment]]()

  def build(
    version: StdLibVersion,
    nByte: Byte,
    in: Coeval[Environment.InputEntity],
    h: Coeval[Int],
    blockchain: Blockchain,
    isTokenContext: Boolean,
    isContract: Boolean,
    address: Environment.Tthis,
    txId: ByteStr,
    useNewPowPrecision: Boolean
  ): Either[String, EvaluationContext[Environment, Id]] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map { ds =>
      val environment = new WavesEnvironment(nByte, in, h, blockchain, address, ds, txId)
      build(ds, environment, useNewPowPrecision)
    }

  def build(
    ds: DirectiveSet,
    environment: Environment[Id],
    useNewPowPrecision: Boolean = true
  ): EvaluationContext[Environment, Id] =
    cache
      .synchronized(
        cache.computeIfAbsent(
          (ds.stdLibVersion, ds),
          _ =>
            PureContext.build(ds.stdLibVersion, useNewPowPrecision).withEnvironment[Environment] |+|
            CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment] |+|
            WavesContext.build(Global, ds)
        )
      )
      .evaluationContext(environment)

}
