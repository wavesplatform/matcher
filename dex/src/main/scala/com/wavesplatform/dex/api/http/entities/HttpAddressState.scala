package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.actors.address.AddressActor.Reply.GetState
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class HttpAddressState(
  @ApiModelProperty regular: Map[Asset, Long],
  @ApiModelProperty reserved: Map[Asset, Long],
  @ApiModelProperty allTradableBalance: Map[Asset, Long],
  @ApiModelProperty unconfirmed: Map[Asset, Long],
  @ApiModelProperty outgoingLeasing: Long,
  @ApiModelProperty notCreatedTxs: Map[String, Map[Asset, Long]],
  @ApiModelProperty notObservedTxs: Map[String, Map[Asset, Long]],
  @ApiModelProperty placementQueue: List[Order.Id]
)

object HttpAddressState {

  implicit val HttpAddressStateFormat: Format[HttpAddressState] = Json.format

  def apply(s: GetState): HttpAddressState = new HttpAddressState(
    s.balances.regular.xs,
    s.balances.reserved.xs,
    s.balances.allTradableBalance.xs,
    s.balances.unconfirmed.xs,
    s.balances.outgoingLeasing.getOrElse(0L),
    s.balances.notCreatedTxs map { case (k, v) => (k.toString, v.xs) },
    s.balances.notObservedTxs map { case (k, v) => (k.toString, v.xs) },
    s.placementQueue.toList
  )

}
