package com.wavesplatform.it.matcher.api.http

import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

trait HttpApiSuiteBase extends MatcherSuiteBase with RawHttpChecks {

  protected def placeAndGetIds(): Set[String] =
    Set(
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
      mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
    ).map { o =>
      placeAndAwaitAtDex(o)
      o.idStr()
    }

  protected def shouldReturnErrorWithoutApiKeyHeader() =
    "should return an error without X-API-KEY" in {

      this.getClass.getSimpleName match {
        case "UpdateRatesByAssetIdSpec" => validateAuthorizationError(dex1.rawApi.upsertRate(UsdId.toString, 0.5))
        case "DeleteRatesSpec" => validateAuthorizationError(dex1.rawApi.deleteRate(UsdId.toString))
        case "GetAllSnapshotOffsetsSpec" => validateAuthorizationError(dex1.rawApi.getAllSnapshotOffsets(Map.empty))
        case "PostSaveSnapshotsSpec" => validateAuthorizationError(dex1.rawApi.saveSnapshots(Map.empty))
        case "GetLastOffsetSpec" => validateAuthorizationError(dex1.rawApi.getLastOffset(Map.empty))
        case "GetCurrentOffsetSpec" => validateAuthorizationError(dex1.rawApi.getCurrentOffset(Map.empty))
        case "GetOldestSnapshotOffsetSpec" => validateAuthorizationError(dex1.rawApi.getOldestSnapshotOffset(Map.empty))
        case "DeleteOrderBookSpec" => validateAuthorizationError(dex1.rawApi.deleteOrderBook("WAVES", UsdId.toString, Map.empty))
        case "GetOrderHistoryByApiKeySpec" =>
          validateAuthorizationError(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, false, true, Map.empty))
        case "GetMatcherConfigSpec" => validateAuthorizationError(dex1.rawApi.getMatcherConfig(Map.empty))
        case "CancelOrderByIdSpec" =>
          val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
          placeAndAwaitAtDex(order)

          validateAuthorizationError(dex1.rawApi.cancelOrderById(order.idStr(), Map.empty[String, String]))
        case "CancelOrdersByAddressAndIdsSpec" =>
          validateAuthorizationError(dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, placeAndGetIds(), Map.empty))
        case "GetOrderStatusInfoByIdWithApiKeySpec" =>
          val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
          placeAndAwaitAtDex(order)
          validateAuthorizationError(
            dex1.rawApi.getOrderStatusInfoById(alice.toAddress.stringRepr, order.idStr(), Map.empty)
          )
        case _ => fail(s"There is no test for ${this.getClass.getSimpleName}")
      }

    }

  protected def shouldReturnErrorWithIncorrectApiKeyValue() =
    "should return an error with incorrect X-API-KEY" in {

      val headers = Map("X-API-KEY" -> "incorrect")

      this.getClass.getSimpleName match {
        case "UpdateRatesByAssetIdSpec" => validateAuthorizationError(dex1.rawApi.upsertRate(UsdId.toString, 0.5, headers))
        case "DeleteRatesSpec" => validateAuthorizationError(dex1.rawApi.deleteRate(UsdId.toString, headers))
        case "GetAllSnapshotOffsetsSpec" => validateAuthorizationError(dex1.rawApi.getAllSnapshotOffsets(headers))
        case "PostSaveSnapshotsSpec" => validateAuthorizationError(dex1.rawApi.saveSnapshots(headers))
        case "GetLastOffsetSpec" => validateAuthorizationError(dex1.rawApi.getLastOffset(headers))
        case "GetCurrentOffsetSpec" => validateAuthorizationError(dex1.rawApi.getCurrentOffset(headers))
        case "GetOldestSnapshotOffsetSpec" => validateAuthorizationError(dex1.rawApi.getOldestSnapshotOffset(headers))
        case "DeleteOrderBookSpec" => validateAuthorizationError(dex1.rawApi.deleteOrderBook("WAVES", UsdId.toString, headers))
        case "GetOrderHistoryByApiKeySpec" =>
          validateAuthorizationError(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr, false, true, headers))
        case "GetMatcherConfigSpec" => validateAuthorizationError(dex1.rawApi.getMatcherConfig(headers))
        case "CancelOrdersByAddressAndIdsSpec" =>
          val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
          placeAndAwaitAtDex(order)

          validateAuthorizationError(
            dex1.rawApi.cancelOrderById(order.idStr(), headers)
          )
        case "CancelOrdersByAddressAndIdsSpec" =>
          validateAuthorizationError(dex1.rawApi.cancelAllByAddressAndIds(alice.toAddress.stringRepr, placeAndGetIds(), headers))
        case "GetOrderStatusInfoByIdWithApiKeySpec" =>
          val order = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd)
          placeAndAwaitAtDex(order)

          validateAuthorizationError(
            dex1.rawApi.getOrderStatusInfoById(
              alice.toAddress.stringRepr,
              order.idStr(),
              Map("X-User-Public-Key" -> Base58.encode(alice.publicKey), "X-API-Key" -> "incorrect")
            )
          )
        case _ => fail(s"There is no test for ${this.getClass.getSimpleName}")
      }
    }

}
