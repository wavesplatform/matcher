package com.wavesplatform.dex.grpc.integration.smart

import cats.syntax.either._
import cats.{Eval, Id}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.{RealTransactionWrapper, Verifier}
import com.wavesplatform.transaction.{Authorized, Proven}

object MatcherScriptRunner {

  def apply(script: Script, order: Order): (Log[Id], Either[String, EVALUATED]) = script match {
    case s: ExprScript =>
      MatcherContext.build(script.stdLibVersion, AddressScheme.current.chainId, Eval.later(order)) match {
        case Left(error) => (List.empty, Left(error))
        case Right(ctx)  => EvaluatorV1.apply().applyWithLogging(ctx, s.expr)
      }

    case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf))) =>
      MatcherContext.build(
        script.stdLibVersion,
        AddressScheme.current.chainId,
        Eval.later(order)
      ) match {
        case Left(error) => (List.empty, Left(error))
        case Right(ctx) =>
          val evalContract = ContractEvaluator.verify(decls, vf, RealTransactionWrapper.ord(order))
          EvaluatorV1.apply().evalWithLogging(ctx, evalContract)
      }

    case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None)) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })

    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
