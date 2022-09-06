package com.wavesplatform.it.sync

import com.wavesplatform.crypto.base.Base58
import com.wavesplatform.dex.api.http.entities.HttpAddressCheck
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.it.MatcherSuiteBase
import org.testcontainers.images.builder.Transferable

import java.nio.charset.StandardCharsets

class CheckAddressTestSuite extends MatcherSuiteBase {

  private val account: String = "6ChCezZ84odRA6T511jbB7GuVvDJSudoKtsotJo4pMQu"

  private val publicKey: PublicKey = PublicKey(Base58.decode(account))

  private val address: Address = publicKey.toAddress

  "CheckAddressTestSuite" - {

    "should check address if it doesn't exists anywhere" in {
      dex1.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = false, blockchain = false))
    }

    "should check address if it only exists in matcher" in {
      dex1.copyFileToContainer(Transferable.of(account.getBytes(StandardCharsets.UTF_8)), filePath = "/lp/accounts")
      dex1.restart()
      eventually {
        dex1.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = false))
      }
    }

    "should check address if it exists everywhere" in {
      wavesNode1.copyFileToContainer(Transferable.of(account.getBytes(StandardCharsets.UTF_8)), filePath = "/lp/accounts")
      wavesNode1.restart()
      eventually {
        dex1.tryApi.checkAddress(address) shouldBe Right(HttpAddressCheck(matcher = true, blockchain = true))
      }
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

}
