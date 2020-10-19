package com.wavesplatform.it.api

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.docker.DexContainer

sealed trait MatcherCommand extends Product with Serializable

object MatcherCommand {
  case class Place(dex: DexContainer, order: Order) extends MatcherCommand
  case class Cancel(dex: DexContainer, owner: KeyPair, order: Order) extends MatcherCommand
}
