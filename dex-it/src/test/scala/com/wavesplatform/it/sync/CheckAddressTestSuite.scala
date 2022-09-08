package com.wavesplatform.it.sync

import com.wavesplatform.crypto.base.Base58
import com.wavesplatform.dex.api.http.entities.HttpAddressCheck
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}
import com.wavesplatform.it.MatcherSuiteBase

class CheckAddressTestSuite extends MatcherSuiteBase {

  import CheckAddressTestSuite._

  "CheckAddressTestSuite" - {

    "should check address if it doesn't exists anywhere" in {
      dex1.tryApi.checkAddress(publicKey1.toAddress) shouldBe Right(HttpAddressCheck(matcher = false, blockchain = false))
    }

    "should check address if it only exists in matcher" in {
      dex1.tryApi.checkAddress(publicKey2.toAddress) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = false))
    }

    "should check address if it only exists in blockchain" in {
      dex1.tryApi.checkAddress(publicKey3.toAddress) shouldBe Right(HttpAddressCheck(matcher = false, blockchain = true))
    }

    "should check address if it exists everywhere" in {
      dex1.tryApi.checkAddress(publicKey4.toAddress) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = true))
    }
  }

  override protected lazy val dex1: DexContainer =
    createDex(name = "dex-1", lpAccounts = Seq(publicKey2, publicKey4))

  override protected lazy val wavesNode1: WavesNodeContainer =
    createWavesNode(name = "waves-1", lpAccounts = Seq(publicKey3, publicKey4))

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

}

object CheckAddressTestSuite {

  private val publicKey1: PublicKey = "6ChCezZ84odRA6T511jbB7GuVvDJSudoKtsotJo4pMQu".toPublicKey
  private val publicKey2: PublicKey = "6Cehbk9Mv8BzubsCG9SPqXgb2uoTDe1hjtgZDHjzyj4U".toPublicKey
  private val publicKey3: PublicKey = "6CeiQEN5zbuEC7n72szgE4LFcMXwcUtzGKMQk9oaGmiZ".toPublicKey
  private val publicKey4: PublicKey = "6ChCf3BqZvTCsy7hBw23hVPrRRJ3XEboxy6JnZ2BsbNh".toPublicKey

  implicit private[this] class RichString(val value: String) extends AnyVal {

    def toPublicKey: PublicKey = PublicKey(Base58.decode(value))
  }

}
