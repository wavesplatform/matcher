package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.actors.address.AddressBalance
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

import scala.collection.immutable.Queue

case class HttpAddressState(
  @ApiModelProperty regular: Map[Asset, Long],
  @ApiModelProperty reserved: Map[Asset, Long],
  @ApiModelProperty allTradableBalance: Map[Asset, Long],
  @ApiModelProperty unconfirmed: Map[Asset, Long],
  @ApiModelProperty outgoingLeasing: Long,
  @ApiModelProperty notCreatedTxs: Map[String, Map[Asset, Long]],
  @ApiModelProperty notObservedTxs: Map[String, Map[Asset, Long]],
  @ApiModelProperty placementQueue: List[String]
)

object HttpAddressState {

  implicit val HttpAddressStateFormat: Format[HttpAddressState] = Json.format

  def apply(balance: AddressBalance, queue: Queue[Order.Id]): HttpAddressState =
    new HttpAddressState(
      balance.regular.xs,
      balance.reserved.xs,
      balance.allTradableBalance.xs,
      balance.unconfirmed.xs,
      balance.outgoingLeasing.getOrElse(0L),
      balance.notCreatedTxs.map(e => e._1.toString -> e._2.xs),
      balance.notObservedTxs.map(e => e._1.toString -> e._2.xs),
      queue.map(_.toString).toList
    )

}
