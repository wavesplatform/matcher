package com.wavesplatform.dex.load.utils

import com.wavesplatform.wavesj.Node

class Services(settings: Settings) {
  val node    = new Node(settings.hosts.node)
  val matcher = new Node(settings.hosts.matcher)
}
