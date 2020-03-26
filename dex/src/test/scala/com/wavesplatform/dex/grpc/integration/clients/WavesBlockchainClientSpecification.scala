package com.wavesplatform.dex.grpc.integration.clients

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class WavesBlockchainClientSpecification extends AnyWordSpecLike with Matchers with MatcherSpecBase {

  "combineBalanceChanges should work correctly" in {

    val alice = mkKeyPair("alice").publicKey.toAddress
    val bob   = mkKeyPair("bob").publicKey.toAddress
    val carol = mkKeyPair("carol").publicKey.toAddress

    val firstChanges: SpendableBalanceChanges =
      Map(
        alice -> Map(Waves -> 10.waves, usd -> 15.usd, btc -> 6.btc),
        bob   -> Map(Waves -> 4.waves)
      )

    val secondChanges: SpendableBalanceChanges =
      Map(
        alice -> Map(Waves -> 37.waves, usd -> 13.usd, eth -> 6.eth),
        bob   -> Map(btc   -> 1.5.btc),
        carol -> Map(btc   -> 2.btc)
      )

    List(firstChanges, secondChanges).reduceLeft(WavesBlockchainClient.combineBalanceChanges) shouldBe Map(
      alice -> Map(Waves -> 37.waves, usd -> 13.usd, btc -> 6.btc, eth -> 6.eth),
      bob   -> Map(Waves -> 4.waves, btc -> 1.5.btc),
      carol -> Map(btc   -> 2.btc)
    )
  }
}
