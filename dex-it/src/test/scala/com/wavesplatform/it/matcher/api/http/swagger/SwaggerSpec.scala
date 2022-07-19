package com.wavesplatform.it.matcher.api.http.swagger

import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class SwaggerSpec extends MatcherSuiteBase with RawHttpChecks {

  "GET /api-docs/swagger.json " - {

    "should return correct version in json schema" in {
      dexVersion should be((validate200Json(dex1.rawApi.apiDocs()) \ "info" \ "version").as[String])
    }
  }
}
