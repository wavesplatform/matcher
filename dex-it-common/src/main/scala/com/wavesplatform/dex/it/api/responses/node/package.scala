package com.wavesplatform.dex.it.api.responses

import com.wavesplatform.transactions.Transaction
import play.api.libs.json.{JsSuccess, Reads}

package object node {

  implicit val transactionReads: Reads[Transaction] = Reads { js =>
    JsSuccess(Transaction.fromJson(js.toString()))
  }

}
