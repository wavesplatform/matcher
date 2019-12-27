// TODO DEX-558
//package com.wavesplatform.it.sync.orders
//
//import java.nio.charset.StandardCharsets
//
//import com.typesafe.config.{Config, ConfigFactory}
//import com.wavesplatform.account.KeyPair
//import com.wavesplatform.common.state.ByteStr
//import com.wavesplatform.dex.settings.AssetType._
//import com.wavesplatform.dex.settings.FeeMode._
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
//import com.wavesplatform.transaction.Asset
//import com.wavesplatform.transaction.Asset.IssuedAsset
//import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
//
//class V3OrderPercentFeeFeeReceivingTestSuite extends OrderPercentFeePriceTestSuite(3.toByte)
//
//abstract class OrderPercentFeeReceivingTestSuite(version: Byte, feeAsset: Asset = IssuedAsset(UsdId)) extends MatcherSuiteBase {
//  val assetType = RECEIVING
//
//  val price                = 1.2.usd
//  val fullyAmountWaves     = 15.waves
//  val partiallyAmountWaves = 9.waves
//  val fullyAmountUsd       = 18.usd
//  val minimalFee           = 4.5.usd
//  val partiallyFeeUsd      = 2.7.usd
//  val partiallyAmountUsd   = 10.8.usd
//  val tooLowFee            = 4.49.usd
//  val tooHighFee           = 18.01.usd
//  val minimalFeeWaves      = 3.75.waves
//  val tooLowFeeWaves       = 3.749999.waves
//  val tooHighFeeWaves      = 15.00001.waves
//  val partiallyFeeWaves    = 2.25.waves
//
//  override protected def nodeConfigs: Seq[Config] = {
//    val orderFeeSettingsStr =
//      s"""
//         |waves.dex {
//         |  allowed-order-versions = [1, 2, 3]
//         |  order-fee {
//         |    mode = $PERCENT
//         |    $PERCENT {
//         |      asset-type = $assetType
//         |      min-fee = $percentFee
//         |    }
//         |  }
//         |}
//       """.stripMargin
//
//    super.nodeConfigs.map(
//      ConfigFactory
//        .parseString(orderFeeSettingsStr)
//        .withFallback
//    )
//  }
//
//  override protected def beforeAll(): Unit = {
//    super.beforeAll()
//    val txIds = Seq(IssueUsdTx).map(_.json()).map(node.broadcastRequest(_).id)
//    txIds.foreach(node.waitForTransaction(_))
//  }
//
//  def createAccountWithBalance(balances: (Long, Option[String])*): KeyPair = {
//    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))
//
//    balances.foreach {
//      case (balance, asset) => {
//        if (asset != None)
//          assert(
//            node.assetBalance(alice.toAddress.toString, asset.get.toString).balance >= balance,
//            s"Bob doesn't have enough balance in ${asset.get.toString} to make a transfer"
//          )
//        node.waitForTransaction(node.broadcastTransfer(alice, account.toAddress.toString, balance, 0.003.waves, asset, None).id)
//      }
//    }
//    account
//  }
//
//  def getBalance(account: KeyPair, asset: String): Long = {
//    if (asset.equals("WAVES"))
//      node.accountBalances(account.toAddress.toString)._1
//    else node.assetBalance(account.toAddress.toString, asset).balance
//  }
//
//  def balancesShouldBe(account: KeyPair, balances: (Long, String)*): Unit = {
//    balances.foreach {
//      case (balance, asset) => {
//        getBalance(account, asset) should be(balance)
//      }
//    }
//  }
//
//  def reservedBalancesShouldBe(account: KeyPair, balances: (Long, String)*): Unit = {
//    balances.foreach {
//      case (balance, asset) => {
//        node.reservedBalance(account).getOrElse(asset, 0) should be(balance)
//      }
//    }
//  }
//
//  s"V$version orders (fee asset type: $assetType) & fees processing" - {
//    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
//      val accountBuyer  = createAccountWithBalance(minimalFeeWaves  -> None, fullyAmountUsd -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(fullyAmountWaves -> None)
//
//      node.waitOrderProcessed(wavesUsdPair, node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(UsdId)).message.id)
//
//      balancesShouldBe(accountBuyer, fullyAmountWaves -> "WAVES", 0L                          -> UsdId.toString)
//      balancesShouldBe(accountSeller, 0L              -> "WAVES", fullyAmountUsd - minimalFee -> UsdId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, 0L  -> UsdId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L -> UsdId.toString, 0L -> "WAVES")
//    }
//
//    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
//      val accountBuyer  = createAccountWithBalance(minimalFeeWaves      -> None, fullyAmountUsd -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(partiallyAmountWaves -> None, minimalFee     -> Some(UsdId.toString))
//
//      node.waitOrderProcessed(wavesUsdPair, node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node
//          .placeOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(UsdId))
//          .message
//          .id)
//
//      balancesShouldBe(accountBuyer, partiallyAmountWaves - (minimalFeeWaves - partiallyFeeWaves) -> "WAVES", partiallyAmountUsd -> UsdId.toString)
//      balancesShouldBe(accountSeller, 0L                                                          -> "WAVES", partiallyAmountUsd -> UsdId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, fullyAmountUsd - partiallyAmountUsd -> UsdId.toString, (minimalFeeWaves - partiallyFeeWaves) -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L                                 -> UsdId.toString, 0L                                    -> "WAVES")
//
//      node.cancelAllOrders(accountBuyer)
//    }
//
//    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + tooHighFeeWaves -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(fullyAmountWaves            -> None, tooHighFee -> Some(UsdId.toString))
//
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, tooHighFeeWaves, version).message.id)
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, tooHighFee, version, feeAsset = IssuedAsset(UsdId)).message.id)
//
//      balancesShouldBe(accountBuyer, fullyAmountWaves -> "WAVES", 0L             -> UsdId.toString)
//      balancesShouldBe(accountSeller, 0L              -> "WAVES", fullyAmountUsd -> UsdId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, 0L  -> UsdId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L -> UsdId.toString, 0L -> "WAVES")
//    }
//
//    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
//      assertBadRequestAndMessage(
//        node.placeOrder(
//          createAccountWithBalance(minimalFeeWaves -> None, fullyAmountUsd -> Some(UsdId.toString)),
//          wavesUsdPair,
//          BUY,
//          fullyAmountWaves,
//          price,
//          tooLowFeeWaves,
//          version,
//          feeAsset = feeAsset
//        ), "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES")
//    }
//
//    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
//      assertBadRequestAndMessage(
//        node.placeOrder(
//          createAccountWithBalance(fullyAmountWaves -> None, minimalFee -> Some(UsdId.toString)),
//          wavesUsdPair,
//          SELL,
//          fullyAmountWaves,
//          price,
//          tooLowFee,
//          version,
//          feeAsset = feeAsset
//        ), "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES")
//    }
//  }
//}
