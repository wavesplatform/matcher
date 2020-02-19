package com.wavesplatform.it.sync.api

import com.wavesplatform.it.MatcherSuiteBase

import scala.collection.JavaConversions._

class MatcherWebSocketsTestSuite extends MatcherSuiteBase {

  "connection should be established" in {
    // open connection to ws-service, connection will be alive until ws.success() will not called
    val wsAlice = mkWebSocket(alice, dex1)
    val wsBob = mkWebSocket(bob, dex1)

    // some actions...
    Thread.sleep(10000)

    //messages should have size 9
    for (e <- messages.entrySet) e.getValue should startWith("Now")

    // close the connection
    wsAlice.success(None)
  }

}
