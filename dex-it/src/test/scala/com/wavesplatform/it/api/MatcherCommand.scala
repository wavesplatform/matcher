package com.wavesplatform.it.api

import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.it.api.dex.DexApi
import com.wavesplatform.dex.domain.order.Order

import scala.concurrent.Future

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(api: DexApi[Future], order: Order)                  extends MatcherCommand
  case class Cancel(api: DexApi[Future], owner: KeyPair, order: Order) extends MatcherCommand
}
