package com.wavesplatform.it.matcher.api.http.swagger

import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class SwaggerSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "GET /api-docs/swagger.json " - {

    "should return correct version in json schema" in {
      dexVersion should be((validate200Json(dex1.rawApi.apiDocs()) \ "info" \ "version").as[String])
    }
  }
}
