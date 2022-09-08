package com.wavesplatform.it.sync

import com.wavesplatform.crypto.base.Base58
import com.wavesplatform.dex.api.http.entities.HttpAddressCheck
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.it.MatcherSuiteBase

class CheckAddressTestSuite extends MatcherSuiteBase {

  private val account: String = "6ChCezZ84odRA6T511jbB7GuVvDJSudoKtsotJo4pMQu"

  private val publicKey: PublicKey = PublicKey(Base58.decode(account))

  private val address: Address = publicKey.toAddress

  private val wavesNode2 = createWavesNode(name = "waves-2", lpAccounts = Seq(publicKey))
  private val dex2 = createDex(name = "dex-2", lpAccounts = Seq(publicKey))

  "CheckAddressTestSuite" - {

    "should check address if it doesn't exists anywhere" in {
      dex1.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = false, blockchain = false))
    }

    "should check address if it only exists in matcher" in {
      dex1.stopWithoutRemove()
      dex2.start()
      eventually {
        dex2.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = false))
      }
    }

    "should check address if it exists everywhere" in {
      wavesNode1.stopWithoutRemove()
      wavesNode2.start()
      eventually {
        dex2.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = true))
      }
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

}
