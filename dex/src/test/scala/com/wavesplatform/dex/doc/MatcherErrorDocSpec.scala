package com.wavesplatform.dex.doc

import com.wavesplatform.dex.util.getSimpleName
import org.scalatest.{FreeSpec, Matchers}

class MatcherErrorDocSpec extends FreeSpec with Matchers {
  "MatcherErrorDoc" - {
    "should not contain two equal error codes" in {
      val samples = MatcherErrorDoc.errorSamples.run.sortBy(_.code)
      samples.zip(samples.tail).foreach {
        case (e1, e2) =>
          withClue(s"${getSimpleName(e1)} and ${getSimpleName(e2)}") {
            e1.code shouldNot be(e2.code)
          }
      }
    }
  }
}
