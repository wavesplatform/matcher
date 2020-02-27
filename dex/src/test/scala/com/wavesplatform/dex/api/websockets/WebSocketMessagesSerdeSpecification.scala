package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.MatcherSpecBase
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class WebSocketMessagesSerdeSpecification extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks with Matchers with MatcherSpecBase {

  private val wsBalancesGen = for {
    tradable <- maxWavesAmountGen
    reserved <- maxWavesAmountGen
  } yield WsBalances(tradable, reserved)

  private val wsAddressStateGen = for {
    changes  <- Gen.choose(1, 5)
    assets   <- Gen.listOfN(changes, assetGen)
    balances <- Gen.listOfN(changes, wsBalancesGen)
  } yield WsAddressState((assets zip balances).toMap)

  "WsAddressState" in forAll(wsAddressStateGen) { as =>
    val json         = WsAddressState.writes.writes(as)
    val addressState = WsAddressState.reads.reads(json).get

    addressState should matchTo(as)
  }
}
