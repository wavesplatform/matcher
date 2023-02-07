package com.wavesplatform.it.sync.metamask

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.it.config.GenesisConfig
import com.wavesplatform.dex.it.metamask.EthTransactionsHelper
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase
import org.web3j.crypto.{Bip32ECKeyPair, Keys}
import org.web3j.utils._

final class EthereumTransactionTestSuite extends MatcherSuiteBase {

  "EthereumTransactionTestSuite" - {

    "waves transfer" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthTransfer(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          5.waves,
          Waves,
          ethTxFee
        )
      wavesNode1.api.broadcastEth(txData)
      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) - 5.waves - ethTxFee
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(Waves) shouldBe bobState1.regular(Waves) + 5.waves
      }
    }

    "usd transfer" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthTransfer(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          12.usd,
          usd,
          ethTxFee
        )
      wavesNode1.api.broadcastEth(txData)
      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) - ethTxFee
        aliceState2.regular(usd) shouldBe aliceState1.regular(usd) - 12.usd
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(usd) shouldBe bobState1.regular(usd) + 12.usd
      }
    }

    "invoke with waves" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthInvoke(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          "withdraw",
          Seq(EthTransactionsHelper.Arg.Bytes(ByteStr.empty), EthTransactionsHelper.Arg.Integer(12.waves)),
          Seq.empty,
          ethTxFee
        )
      wavesNode1.api.broadcastEth(txData)
      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) + 12.waves - ethTxFee
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(Waves) shouldBe bobState1.regular(Waves) - 12.waves
      }
    }

    "invoke with usd" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthInvoke(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          "withdraw",
          Seq(EthTransactionsHelper.Arg.Bytes(UsdId), EthTransactionsHelper.Arg.Integer(15.usd)),
          Seq.empty,
          ethTxFee
        )
      wavesNode1.api.broadcastEth(txData)
      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) - ethTxFee
        aliceState2.regular(usd) shouldBe aliceState1.regular(usd) + 15.usd
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(usd) shouldBe bobState1.regular(usd) - 15.usd
      }
    }

    "invoke with payment" in {
      val aliceState1 = dex1.api.getAddressState(aliceEthWavesAdr)
      val bobState1 = dex1.api.getAddressState(bob)
      val (_, txData) =
        EthTransactionsHelper.generateEthInvoke(
          GenesisConfig.chainId,
          aliceEthKeyPair,
          bob,
          "withdraw",
          Seq(EthTransactionsHelper.Arg.Bytes(ByteStr.empty), EthTransactionsHelper.Arg.Integer(0L)),
          Seq(
            EthTransactionsHelper.Payment(6.waves, Waves),
            EthTransactionsHelper.Payment(3.usd, usd)
          ),
          ethTxFee
        )
      wavesNode1.api.broadcastEth(txData)
      eventually {
        val aliceState2 = dex1.api.getAddressState(aliceEthWavesAdr)
        aliceState2.regular(Waves) shouldBe aliceState1.regular(Waves) - 6.waves - ethTxFee
        aliceState2.regular(usd) shouldBe aliceState1.regular(usd) - 3.usd
        val bobState2 = dex1.api.getAddressState(bob)
        bobState2.regular(Waves) shouldBe bobState1.regular(Waves) + 6.waves
        bobState2.regular(usd) shouldBe bobState1.regular(usd) + 3.usd
      }
    }

  }

  private lazy val ethTxFee = 0.005.waves

  private lazy val aliceEthKeyPair = Bip32ECKeyPair.generateKeyPair(alice.seed)

  private lazy val aliceEthWavesAdr = Address.fromPublicKeyHash(
    Numeric.hexStringToByteArray(Keys.getAddress(aliceEthKeyPair)),
    GenesisConfig.chainId
  )

  /*
  {-# STDLIB_VERSION 4 #-}
  {-# SCRIPT_TYPE ACCOUNT #-}
  {-# CONTENT_TYPE DAPP #-}

  @Callable (i)
  func withdraw(assetId: ByteVector, amount: Int) = {
    if (assetId.size() == 0) then {
      [ScriptTransfer(i.caller, amount, unit)]
    } else {
      [ScriptTransfer(i.caller, amount, assetId)]
    }
  }
   */
  private lazy val script = Scripts.fromBase64(
    "AAIEAAAAAAAAAAgIAhIECgICAQAAAAAAAAABAAAAAWkBAAAACHdpdGhkcmF3AAAAAgAAAAdhc3NldElkAAAABmFtb3VudAMJAAAAAAAAAgkAAMgAAA" +
    "ABBQAAAAdhc3NldElkAAAAAAAAAAAACQAETAAAAAIJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMIBQAAAAFpAAAABmNhbGxlcgUAAAAGYW1vdW50BQ" +
    "AAAAR1bml0BQAAAANuaWwJAARMAAAAAgkBAAAADlNjcmlwdFRyYW5zZmVyAAAAAwgFAAAAAWkAAAAGY2FsbGVyBQAAAAZhbW91bnQFAAAAB2F" +
    "zc2V0SWQFAAAAA25pbAAAAAAw6igL"
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(
      IssueUsdTx,
      mkSetAccountScript(bob, script)
    )
    broadcastAndAwait(
      mkTransfer(alice, aliceEthWavesAdr, 15.waves, Waves),
      mkTransfer(alice, aliceEthWavesAdr, 15.usd, usd),
      mkTransfer(alice, bob, 10000.usd, usd)
    )
    dex1.start()
  }

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  allowed-order-versions = [1,2,3]
         |}""".stripMargin
    )

}
