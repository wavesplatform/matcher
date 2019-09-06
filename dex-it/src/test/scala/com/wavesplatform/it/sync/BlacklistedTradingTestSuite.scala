package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.Address
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{MatcherError, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.scalatest._

class BlacklistedTradingTestSuite extends NewMatcherSuiteBase with GivenWhenThen {

  import BlacklistedTradingTestSuite._

  override protected def dex1Config: Config = configWithBlacklisted().withFallback(super.dex1Config)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx)
  }

  "When blacklists are empty" in {
    val (dec2, dec8) = (1000L, 1000000000L)

    Then("Place some orders")
    val usdOrder  = mkOrder(alice, matcher, wavesUsdPair, BUY, dec8, dec2)
    val wctOrder  = mkOrder(alice, matcher, wctWavesPair, BUY, dec2, dec8)
    val ethOrder  = mkOrder(alice, matcher, ethWavesPair, SELL, dec8, dec8)
    val btcOrder1 = mkOrder(bob, matcher, wavesBtcPair, SELL, dec8, dec8)
    List(usdOrder, wctOrder, ethOrder, btcOrder1).foreach(dex1Api.place)
    dex1Api.waitForOrderStatus(btcOrder1, OrderStatus.Accepted)

    Then("We blacklist some assets and addresses and restart the node")
    replaceLocalConfig(
      dex1Container(),
      configWithBlacklisted(
        assets = Array(WctId.toString),
        names = Array("ETH.*"),
        addresses = Array(bob.toAddress.toString)
      )
    )
    restartContainer(dex1Container(), dex1Api)

    Then("orders for blacklisted assets are not available and new orders can't be placed")
    testOrderStatusDenied(wctOrder, IssuedAsset(WctId))
    testOrderStatusDenied(ethOrder, IssuedAsset(EthId))

    testOrderPlacementDenied(mkOrder(alice, matcher, wctWavesPair, BUY, dec2, dec8), IssuedAsset(WctId))
    testOrderPlacementDenied(mkOrder(alice, matcher, ethWavesPair, SELL, dec8, dec8), IssuedAsset(EthId))

    testOrderPlacementDenied(mkOrder(bob, matcher, wavesBtcPair, SELL, dec8, dec8), bob)

    And("orders of blacklisted address are still available")
    dex1Api.orderStatus(btcOrder1).status shouldBe OrderStatus.Accepted

    And("orders for other assets are still available")
    dex1Api.orderStatus(usdOrder).status shouldBe OrderStatus.Accepted

    And("OrderBook for blacklisted assets is not available")
    testOrderBookDenied(wctWavesPair, IssuedAsset(WctId))
    testOrderBookDenied(ethWavesPair, IssuedAsset(EthId))
    dex1Api.orderBook(wavesBtcPair).asks.size shouldBe 1

    And("OrderHistory returns info about all orders")
    val aliceOrderHistory = dex1Api.orderHistory(alice, activeOnly = Some(true))
    aliceOrderHistory.size shouldBe 3
    aliceOrderHistory.foreach(_.status shouldBe OrderStatus.Accepted)

    val bobOrderHistory = dex1Api.orderHistory(bob, activeOnly = Some(true))
    bobOrderHistory.size shouldBe 1
    bobOrderHistory.head.status shouldBe OrderStatus.Accepted

    And("Trading markets have info about all asset pairs")
    dex1Api.allOrderBooks.markets.size shouldBe 4

    And("balances are still reserved")
    dex1Api.reservedBalance(alice).size shouldBe 3
    dex1Api.reservedBalance(bob).size shouldBe 1

    And("orders for other assets are still available")
    dex1Api.orderStatus(usdOrder).status shouldBe OrderStatus.Accepted

    And("order can be placed on allowed pair with blacklisted asset")
    val btcOrder2 = mkOrder(alice, matcher, wavesBtcPair, SELL, dec8, dec8)
    dex1Api.place(btcOrder2)
    dex1Api.waitForOrderStatus(btcOrder2, OrderStatus.Accepted)

    And("now if all blacklists are cleared")
    replaceLocalConfig(dex1Container(), configWithBlacklisted())
    restartContainer(dex1Container(), dex1Api)

    Then("OrderBook for blacklisted assets is available again")
    dex1Api.orderBook(wctWavesPair).bids.size shouldBe 1
    dex1Api.orderBook(ethWavesPair).asks.size shouldBe 1

    And("order statuses are available again")
    dex1Api.orderStatus(wctOrder).status shouldBe OrderStatus.Accepted
    dex1Api.orderStatus(ethOrder).status shouldBe OrderStatus.Accepted

    And("new orders can be placed")
    val newWctOrder = mkOrder(alice, matcher, wctWavesPair, BUY, dec2, dec8)
    val newEthOrder = mkOrder(alice, matcher, ethWavesPair, SELL, dec8, dec8)
    val btcOrder3   = mkOrder(bob, matcher, wavesBtcPair, SELL, dec8, dec8)
    val newOrders   = List(newWctOrder, newEthOrder, btcOrder3)
    newOrders.foreach(dex1Api.place)
    newOrders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))
  }

  private def testOrderPlacementDenied(order: Order, address: Address): Unit =
    dex1Api.tryPlace(order) should failWith(3145733, MatcherError.Params(address = Some(address.stringRepr)))

  private def testOrderPlacementDenied(order: Order, blacklistedAsset: Asset): Unit =
    failedDueAssetBlacklist(dex1Api.tryPlace(order), order.assetPair, blacklistedAsset)

  private def testOrderStatusDenied(order: Order, blacklistedAsset: Asset): Unit =
    failedDueAssetBlacklist(dex1Api.tryOrderStatus(order), order.assetPair, blacklistedAsset)

  private def testOrderBookDenied(assetPair: AssetPair, blacklistedAsset: Asset): Unit =
    failedDueAssetBlacklist(dex1Api.tryOrderBook(assetPair), assetPair, blacklistedAsset)

  private def failedDueAssetBlacklist(r: Either[MatcherError, Any], assetPair: AssetPair, blacklistedAsset: Asset) =
    r should failWith(expectedErrorCode(assetPair, blacklistedAsset), MatcherError.Params(assetId = Some(AssetPair.assetIdStr(blacklistedAsset))))

  private def expectedErrorCode(assetPair: AssetPair, blacklistedAsset: Asset): Int =
    if (blacklistedAsset == assetPair.amountAsset) 11538181 // AmountAssetBlacklisted
    else 11538437 // PriceAssetBlacklisted
}

object BlacklistedTradingTestSuite {

  def configWithBlacklisted(assets: Array[String] = Array.empty,
                            names: Array[String] = Array.empty,
                            addresses: Array[String] = Array.empty,
                            allowedAssetPairs: Array[String] = Array.empty): Config = {
    def toStr(array: Array[String]): String = if (array.length == 0) "" else array.mkString("\"", "\", \"", "\"")
    parseString(s"""
                |waves.dex {
                |  blacklisted-assets = [${toStr(assets)}]
                |  blacklisted-names = [${toStr(names)}]
                |  blacklisted-addresses = [${toStr(addresses)}]
                |  allowed-asset-pairs = [${toStr(allowedAssetPairs)}]
                |  white-list-only = no
                |}
    """.stripMargin)
  }

}
