package com.wavesplatform.it.sync.api

import com.wavesplatform.dex.it.api.dex.WebSocket
import com.wavesplatform.it.MatcherSuiteBase

import scala.collection.JavaConversions._

class MatcherWebSocketsTestSuite extends MatcherSuiteBase {

  "connection should be established" in {
    // open connection to ws-service, connection will be alive until ws.success() will not called
    val wsAlice = new WebSocket(alice, dex1)

    // some actions...
    Thread.sleep(10000)

    wsAlice.messages should have size 9
    for (e <- wsAlice.messages.entrySet) e.getValue should startWith("Now")

    // close the connection
    wsAlice.success()
  }

}
