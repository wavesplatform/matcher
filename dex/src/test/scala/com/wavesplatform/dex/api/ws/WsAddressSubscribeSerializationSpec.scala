package com.wavesplatform.dex.api.ws

import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.api.ws.entities.WsAddressFlag.ExcludeNft
import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe
import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe.wsAddressSubscribeFormat
import com.wavesplatform.dex.domain.account.Address
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

class WsAddressSubscribeSerializationSpec extends AnyWordSpec with Matchers with MatcherSpecBase {

  val baseWsSubscribe = WsAddressSubscribe(
    Address.fromString("3N93cuB7hDLhpg8n6QpyV7vbWaj5qwBXDF4").getOrElse(throw new RuntimeException("incorrect address")),
    "jwt",
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzaWciOiIzamRBeTg2WTVKSzg3RWVCM1pqdFdhTWJXUmYyOGlFUThmNmY0NGttQXRBRVlOWDJzbjN2V2taVExFb291WEFFdjF2Y3FqZXZid2p1eXRmeWJNdEVCVDNkIiwibmIiOiJXIiwidXNlcl9uYW1lIjoiNUE2TkJ4eTJNWXN5cmNuemgyVDdncEZoS0JVRnRtVXNTb0pNRHA3OExlcnMiLCJzY29wZSI6WyJnZW5lcmFsIl0sImx0IjoxNzY0LCJwayI6IjVBNk5CeHkyTVlzeXJjbnpoMlQ3Z3BGaEtCVUZ0bVVzU29KTURwNzhMZXJzIiwiZXhwIjoxNTg4MDU0MDg3LCJqdGkiOiJkNWM5M2Y2ZS05YTEwLTQ4ZTYtOTRkYi05NzhjYmI4MzgxMWUiLCJjaWQiOiJkZWZhdWx0LWNsaWVudCJ9.3Lwt0Akq2Xeg_UaDJHXlOq-D4kwpfdpMCTNuDG0IEMg"
  )

  "WsAddressSubscribeSerialization" should {

    "accept WsAddressSubscribe with -nft filter" in {
      val jsonMessage =
        s"""
           |{
           |  "T": "aus",
           |  "S": "${baseWsSubscribe.key.stringRepr}",
           |  "t": "${baseWsSubscribe.authType}",
           |  "j": "${baseWsSubscribe.jwt}",
           |  "b": {
           |    "f": ["-nft"]
           |  }
           |}
           |""".stripMargin

      wsAddressSubscribeFormat.reads(Json.parse(jsonMessage)).get shouldBe baseWsSubscribe.copy(flags = Set(ExcludeNft))
    }

    "accept WsAddressSubscribe with empty filters" in {
      val jsonMessage =
        s"""
           |{
           |  "T": "aus",
           |  "S": "${baseWsSubscribe.key.stringRepr}",
           |  "t": "${baseWsSubscribe.authType}",
           |  "j": "${baseWsSubscribe.jwt}",
           |  "b": {
           |    "f": []
           |  }
           |}
           |""".stripMargin

      wsAddressSubscribeFormat.reads(Json.parse(jsonMessage)).get shouldBe baseWsSubscribe
    }

    "accept WsAddressSubscribe without \"f\" key" in {
      val jsonMessage =
        s"""
           |{
           |  "T": "aus",
           |  "S": "${baseWsSubscribe.key.stringRepr}",
           |  "t": "${baseWsSubscribe.authType}",
           |  "j": "${baseWsSubscribe.jwt}",
           |  "b": {
           |  }
           |}
           |""".stripMargin

      wsAddressSubscribeFormat.reads(Json.parse(jsonMessage)).get shouldBe baseWsSubscribe
    }

    "accept WsAddressSubscribe without \"b\" key" in {
      val jsonMessage =
        s"""
           |{
           |  "T": "aus",
           |  "S": "${baseWsSubscribe.key.stringRepr}",
           |  "t": "${baseWsSubscribe.authType}",
           |  "j": "${baseWsSubscribe.jwt}"
           |}
           |""".stripMargin

      wsAddressSubscribeFormat.reads(Json.parse(jsonMessage)).get shouldBe baseWsSubscribe
    }

  }

}
