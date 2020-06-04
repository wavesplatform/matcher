package com.wavesplatform.dex.load

import com.wavesplatform.wavesj.Node

class Services(settings: Settings) {
  val node    = new Node(settings.hosts.node, settings.networkByte.charAt(0).toByte)
  val matcher = new Node(settings.hosts.matcher, settings.networkByte.charAt(0).toByte)
}
