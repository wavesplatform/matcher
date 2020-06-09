package com.wavesplatform.dex.load.utils

import com.wavesplatform.wavesj.Node

class Services(settings: Settings) {
  val node    = new Node(settings.hosts.node, settings.chainId.charAt(0).toByte)
  val matcher = new Node(settings.hosts.matcher, settings.chainId.charAt(0).toByte)
}
