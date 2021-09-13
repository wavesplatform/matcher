package com.wavesplatform.it.matcher.api.http.cancel

import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.{HttpError, HttpSuccessfulSingleCancel}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.error.{InvalidAddress, InvalidJson, OrderNotFound}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks
import sttp.model.StatusCode

class CancelOrdersByAddressAndIdsSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  def placeOrders(): Set[Order] =
    Set(
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
    ).map { o =>
      placeAndAwaitAtDex(o)
      o
    }

  val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)

  def mkHeaders(pk: PublicKey, ts: Long, sign: ByteStr) =
    Map("X-User-Public-Key" -> pk.toString, "Timestamp" -> ts.toString, "Signature" -> sign.toString)

  "POST /matcher/orders/{address}/cancel" - {
    "should cancel orders by ids" in {

      val orders = placeOrders()
      val r = validate200Json(dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(alice.toAddress.stringRepr, orders.map(_.idStr())))

      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size orders.size

      r.message.foreach { m =>
        m.foreach {
          case util.Right(HttpSuccessfulSingleCancel(_, success, status)) => success should be(true); status should be("OrderCanceled")
          case _ => fail(s"Unexpected response $r")
        }
      }

      orders.foreach(dex1.api.waitForOrderStatus(_, Status.Cancelled))
    }

    "should cancel orders by public key and signature" in {
      val orders = placeOrders()

      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      val r = validate200Json(dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(
        alice.toAddress.stringRepr,
        orders.map(_.idStr()),
        mkHeaders(alice.publicKey, ts, sign)
      ))

      r.success should be(true)
      r.status should be("BatchCancelCompleted")
      r.message.head should have size orders.size

      r.message.foreach { m =>
        m.foreach {
          case util.Right(HttpSuccessfulSingleCancel(_, success, status)) => success should be(true); status should be("OrderCanceled")
          case _ => fail(s"Unexpected response $r")
        }
      }

    }

    "should return OK if there is nothing to cancel" in {
      validate200Json(dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(alice.stringRepr, Set.empty[String]))
    }

    "should return an error when one of ids is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(alice.stringRepr, placeAndGetIds(3) + "null"),
        StatusCode.BadRequest,
        InvalidJson.code,
        "The provided JSON contains invalid fields: (3). Check the documentation"
      )
    }

    "should return an error when address is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature("null", placeAndGetIds(3)),
        StatusCode.BadRequest,
        InvalidAddress.code,
        "Provided address is not correct, reason: Unable to decode base58: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return order not found if address path parameter and publicKey header are from different accounts" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      val r =
        validate200Json(dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(bob.stringRepr, Set(o.idStr()), mkHeaders(alice.publicKey, ts, sign)))

      r.message.head should have size 1
      r.message.head.foreach {
        case util.Left(HttpError(e, m, _, _, _, _)) => e shouldBe OrderNotFound.code; m shouldBe s"The order ${o.idStr()} not found"
        case _ => fail(s"Unexpected response $r")
      }
    }

    "should return an error if timestamp header has the different value of used in signature" in {
      val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
      placeAndAwaitAtDex(o)
      val ts = System.currentTimeMillis
      val sign = crypto.sign(alice, alice.publicKey ++ Longs.toByteArray(ts))

      validateIncorrectSignature(dex1.rawApi.cancelOrdersByIdsWithKeyOrSignature(
        bob.stringRepr,
        Set(o.idStr()),
        mkHeaders(alice.publicKey, ts + 1000, sign)
      ))
    }
  }

}
