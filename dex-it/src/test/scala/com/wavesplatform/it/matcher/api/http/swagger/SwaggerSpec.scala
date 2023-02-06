package com.wavesplatform.it.matcher.api.http.swagger

import com.wavesplatform.dex.Version.VersionTuple
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexMultipleVersions

@DexMultipleVersions
class SwaggerSpec extends MatcherSuiteBase with RawHttpChecks {

  "GET /api-docs/swagger.json " - {

    "should return correct version in json schema" in {
      val version = (validate200Json(dex1.rawApi.apiDocs()) \ "info" \ "version").as[String]
      version should startWith(s"${VersionTuple._1}.${VersionTuple._2}.${VersionTuple._3}")
    }
  }
}
