package com.wavesplatform.dex.doc

import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.meta.DescendantSamples
import com.wavesplatform.dex.util.getSimpleName
import play.api.libs.json.Json

object MatcherErrorDoc {
  object samples extends DescendantSamples[MatcherError]

  def mkMarkdown: String = {
    val items = samples.run
      .sortBy(_.code)
      .map { x =>
        (x, getSimpleName(x))
      }

    val brief = items
      .map {
        case (x, name) => s"| ${x.code} | [$name](#$name) | ${x.message.template}"
      }
      .mkString("\n")

    val detailed = items
      .map {
        case (x, name) =>
          s""">### $name
              > <a id="#$name"></a>
              >```json
              >${Json.prettyPrint(x.json)}
              >```
              > """.stripMargin('>')
      }
      .mkString("\n")

    s""">## List of error codes
        >
        >| Code | Name | Template |
        >|-----:|:-----|----------|
        >$brief
        >
        >## Samples
        >
        >$detailed""".stripMargin('>')
  }
}
