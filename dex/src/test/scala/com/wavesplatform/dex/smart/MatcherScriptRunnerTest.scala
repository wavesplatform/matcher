package com.wavesplatform.dex.smart

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.error.ProduceError.produce
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType, OrderV1}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{FreeSpecLike, Matchers}

import scala.util.Try

class MatcherScriptRunnerTest extends FreeSpecLike with Matchers with TransactionGen with NoShrink {

  private val estimator = ScriptEstimatorV2

  private val sampleOrder = OrderV1(
    sender = KeyPair("test".getBytes()),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Waves, IssuedAsset(ByteStr("asset".getBytes("utf-8")))),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = 30000L
  )

  private def run(script: Script): (Log, Either[String, Terms.EVALUATED]) = MatcherScriptRunner(script, sampleOrder)

  "dApp sunny day" in {
    run(dAppScriptSunny)._2.explicitGet() shouldBe Terms.FALSE
  }

  "Blockchain functions are disabled in dApp" in {
    Try(run(dAppScriptBlockchain)).toEither should produce("""An access to the blockchain\.accountData is denied on DEX""".r)
  }

  private def dAppScriptSunny: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |let addr = addressFromPublicKey(base58'H1kGVNdJwV7N5dF73YWs1R8uet6g5bCvTHmTnYy1hSnr')
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => o.sender == addr
            |      case _ => false
            |    }
            |}
            |""".stripMargin,
        estimator
      )
      .explicitGet()
      ._1

  private def dAppScriptBlockchain: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => getBooleanValue(o.sender, "foo")
            |      case _ => false
            |    }
            |}
            |""".stripMargin,
        estimator
      )
      .explicitGet()
      ._1
}
