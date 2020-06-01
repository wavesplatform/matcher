package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiMatcherPublicKeySpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json             = """"J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf""""
  private val matcherPublicKey = ApiMatcherPublicKey(PublicKey.fromBase58String("J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf").right.get)

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiMatcherPublicKey] should matchTo(matcherPublicKey)
    }

    "serialization" in {
      Json.toJson(matcherPublicKey).toString should matchTo(json)
    }
  }
}
