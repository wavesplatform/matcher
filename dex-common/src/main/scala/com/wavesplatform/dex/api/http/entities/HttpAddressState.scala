package com.wavesplatform.dex.api.http.entities

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

}
