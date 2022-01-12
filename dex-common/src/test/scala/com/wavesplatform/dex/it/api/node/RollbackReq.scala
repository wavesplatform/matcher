package com.wavesplatform.dex.it.api.node

import play.api.libs.json.{Format, Json}

case class RollbackReq(rollbackTo: Int, returnTransactionsToUtx: Boolean)

object RollbackReq {
  implicit val rollbackFormat: Format[RollbackReq] = Json.format
}
