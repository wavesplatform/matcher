package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.CanNotPersistEvent
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpSuccessfulBatchCancelSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  // Note, we removed "result" : null , because it effectively equal to a missing field
  private val json =
    """{
      |  "message" : [ [ {
      |    "orderId" : "8D36dK4snBwJHH9qfDyGo6xP5C4rCH2JPhPbbaJn5mLK",
      |    "success" : true,
      |    "status" : "OrderCanceled"
      |  }, {
      |    "error" : 25601,
      |    "message" : "Can not persist command, please retry later or contact with the administrator",
      |    "template" : "Can not persist command, please retry later or contact with the administrator",
      |    "status" : "OrderCancelRejected",
      |    "success" : false
      |  } ] ],
      |  "success" : true,
      |  "status" : "BatchCancelCompleted"
      |}""".stripMargin

  private val message = HttpSuccessfulBatchCancel(
    List(
      Right(HttpSuccessfulSingleCancel(orderId = ByteStr.decodeBase58("8D36dK4snBwJHH9qfDyGo6xP5C4rCH2JPhPbbaJn5mLK").get)),
      Left(
        HttpError(
          error = CanNotPersistEvent.code,
          message = "Can not persist command, please retry later or contact with the administrator",
          template = "Can not persist command, please retry later or contact with the administrator",
          status = "OrderCancelRejected"
        )
      )
    )
  )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpSuccessfulBatchCancel] should matchTo(message)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
