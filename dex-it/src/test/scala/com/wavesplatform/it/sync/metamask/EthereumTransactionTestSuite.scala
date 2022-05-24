package com.wavesplatform.it.sync.metamask

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.it.MatcherSuiteBase
import org.web3j.crypto.{Bip32ECKeyPair, Keys}
import org.web3j.utils._

class EthereumTransactionTestSuite extends MatcherSuiteBase {

  "EthereumTransactionTestSuite" - {

    "should do eth waves transfer & update address balance properly" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthTransfer(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          5.waves,
          Waves,
          0.005.waves
        )
      wavesNode1.api.broadcastEth(txData)

      //TODO waitForTransaction can replace eventually here
      //but it doesn't work with eth transactions
      //need to wait wavej update

      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) - 5.waves - 0.005.waves
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(Waves) shouldBe bobState1.regular(Waves) + 5.waves
      }
    }
  }

  lazy val aliceEthKeyPair = Bip32ECKeyPair.generateKeyPair(alice.seed)

  lazy val aliceEthWavesAdr = Address.fromPublicKeyHash(
    Numeric.hexStringToByteArray(Keys.getAddress(aliceEthKeyPair)),
    GenesisConfig.chainId
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(
      IssueUsdTx,
      mkTransfer(alice, aliceEthWavesAdr, 15.waves, Waves)
    )
    dex1.start()
  }

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  allowed-order-versions = [1,2,3,4]
         |}""".stripMargin
    )

}
