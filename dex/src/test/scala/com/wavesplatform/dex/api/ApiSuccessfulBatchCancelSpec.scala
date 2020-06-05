package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiSuccessfulBatchCancelSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  // Note, we removed "result" : null , because it effectively equal to a missing field
  private val json =
    """{
      |  "message" : [ [ {
      |    "orderId" : "8D36dK4snBwJHH9qfDyGo6xP5C4rCH2JPhPbbaJn5mLK",
      |    "success" : true,
      |    "status" : "OrderCanceled"
      |  }, {
      |    "error" : 25601,
      |    "message" : "Can not persist event, please retry later or contact with the administrator",
      |    "template" : "Can not persist event, please retry later or contact with the administrator",
      |    "status" : "OrderCancelRejected",
      |    "success" : false
      |  } ] ],
      |  "success" : true,
      |  "status" : "BatchCancelCompleted"
      |}""".stripMargin

  private val message = ApiSuccessfulBatchCancel(
    List(
      Right(ApiSuccessfulSingleCancel(orderId = ByteStr.decodeBase58("8D36dK4snBwJHH9qfDyGo6xP5C4rCH2JPhPbbaJn5mLK").get)),
      Left(
        ApiError(
          error = 25601,
          message = "Can not persist event, please retry later or contact with the administrator",
          template = "Can not persist event, please retry later or contact with the administrator",
          status = "OrderCancelRejected"
        )
      )
    )
  )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiSuccessfulBatchCancel] should matchTo(message)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
