package com.wavesplatform.it.matcher.api.http

import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookInfoTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  val fractions =
    Table(
      ("n", "d"), // First tuple defines column names
      (1, 2), // Subsequent tuples define the data
      (-1, 2),
      (1, -2),
      (-1, -2),
      (3, 1),
      (-3, 1),
      (-3, 0),
      (3, -1),
      (3, Integer.MIN_VALUE),
      (Integer.MIN_VALUE, 3),
      (-3, -1)
    )

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/info " - {

    forAll(fractions) { (a: Int, b: Int) =>

      s"sdsad $a sadas $b" in {
        a should be > -1000000
      }
    }
  }

}
