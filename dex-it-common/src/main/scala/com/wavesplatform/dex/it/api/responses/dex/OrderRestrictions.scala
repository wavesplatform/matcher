package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class OrderRestrictions(minAmount: String, maxAmount: String, stepAmount: String, minPrice: String, maxPrice: String, stepPrice: String)
object OrderRestrictions {
  implicit val orderRestrictions: Format[OrderRestrictions] = Json.format
}
