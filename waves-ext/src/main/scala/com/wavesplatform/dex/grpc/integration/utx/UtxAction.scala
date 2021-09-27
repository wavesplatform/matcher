package com.wavesplatform.dex.grpc.integration.utx

import com.wavesplatform.dex.grpc.integration.services.UtxEvent
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

sealed trait UtxAction extends Product with Serializable
final case class Broadcast(tx: ExchangeTransaction) extends UtxAction
final case class Notify(event: UtxEvent) extends UtxAction
