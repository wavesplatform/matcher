package com.wavesplatform.it.sync.api

import akka.http.scaladsl.model.ws.Message
import com.wavesplatform.it.MatcherSuiteBase

class MatcherWebSocketsTestSuite extends MatcherSuiteBase {

  "connection should be established" in {

    val wsUri                           = s"${dex1.restApiAddress.getPort}/ws/time"
    val outputParser: Message => String = _.asTextMessage.getStrictText

    val wscMobile = mkWebSocketConnection(wsUri, outputParser)
    val wscWeb    = mkWebSocketConnection(wsUri, outputParser, trackOutput = false)
    val wscTest   = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(2000)

    val wscDesktop = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(3000)

    Seq(wscMobile, wscDesktop, wscTest).foreach { connection =>
      connection.close()
      connection.getMessagesBuffer.foreach(_ should startWith("Now is"))
    }

    wscTest.clearMessagesBuffer()

    wscMobile.getMessagesBuffer.size should be > wscDesktop.getMessagesBuffer.size
    Seq(wscWeb, wscTest).foreach { _.getMessagesBuffer.size shouldBe 0 }
  }
}
