// TODO DEX-558
//package com.wavesplatform.it.sync.orders
//
//import java.nio.charset.StandardCharsets
//
//import com.typesafe.config.{Config, ConfigFactory}
//import com.wavesplatform.dex.domain.account.KeyPair
//import com.wavesplatform.common.state.ByteStr
//import com.wavesplatform.dex.settings.AssetType._
//import com.wavesplatform.dex.settings.FeeMode._
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
//import com.wavesplatform.dex.domain.asset.Asset
//import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
//import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
//
//class V3OrderPercentFeeSpendingTestSuite extends OrderPercentFeeSpendingTestSuite(3.toByte)
//
//abstract class OrderPercentFeeSpendingTestSuite(version: Byte, feeAsset: Asset = IssuedAsset(UsdId)) extends MatcherSuiteBase {
//  val assetType = SPENDING
//
//  val price                = 1.2.usd
//  val fullyAmountWaves     = 15.waves
//  val partiallyAmountWaves = 9.waves
//  val fullyAmountUsd       = 18.usd
//  val minimalFee           = 4.5.usd
//  val minimalFeeWaves      = 3.75.waves
//  val tooHighFeeWaves      = 15.00001.waves
//  val tooLowFeeWaves       = 3.7499999.waves
//  val partiallyFeeUsd      = 2.7.usd
//  val partiallyAmountUsd   = 10.8.usd
//  val tooLowFee            = 4.49.usd
//  val tooHighFee           = 4.51.usd
//
//  override protected def nodeConfigs: Seq[Config] = {
//    val orderFeeSettingsStr =
//      s"""
//         |waves.dex {
//         |  allowed-order-versions = [1, 2, 3]
//         |  order-fee {
//         |    mode = $DYNAMIC
//         |    $DYNAMIC {
//         |      base-fee = 300000
//         |    }
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
//    docker.restartNode(node, ConfigFactory.parseString(s"waves.dex.order-fee.mode = $PERCENT"))
//
//    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee        -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> None)
//
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(UsdId)).message.id)
//      node.waitOrderProcessed(wavesUsdPair,
//                              node.placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, minimalFeeWaves, version).message.id)
//
//      balancesShouldBe(accountBuyer, fullyAmountWaves -> "WAVES", 0L             -> UsdId.toString)
//      balancesShouldBe(accountSeller, 0L              -> "WAVES", fullyAmountUsd -> UsdId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, 0L  -> UsdId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L -> UsdId.toString, 0L -> "WAVES")
//    }
//
//    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee            -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(partiallyAmountWaves + minimalFeeWaves -> None)
//
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(UsdId)).message.id)
//      node.waitOrderProcessed(wavesUsdPair,
//                              node
//                                .placeOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, minimalFeeWaves, version)
//                                .message
//                                .id)
//
//      balancesShouldBe(accountBuyer, partiallyAmountWaves -> "WAVES", partiallyAmountUsd - (minimalFee - partiallyFeeUsd) -> UsdId.toString)
//      balancesShouldBe(accountSeller, 0L                  -> "WAVES", partiallyAmountUsd                                  -> UsdId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, fullyAmountUsd - partiallyAmountUsd + (minimalFee - partiallyFeeUsd) -> UsdId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L                                                                  -> UsdId.toString, 0L -> "WAVES")
//
//      node.cancelAllOrders(accountBuyer)
//    }
//
//    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee        -> Some(UsdId.toString))
//      val accountSeller = createAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> None)
//
//      node.waitOrderProcessed(
//        wavesUsdPair,
//        node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(UsdId)).message.id)
//      node.waitOrderProcessed(wavesUsdPair,
//                              node.placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, tooHighFeeWaves, version).message.id)
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
//          createAccountWithBalance(fullyAmountUsd + minimalFee -> Some(UsdId.toString)),
//          wavesUsdPair,
//          BUY,
//          fullyAmountWaves,
//          price,
//          tooLowFee,
//          version,
//          feeAsset = feeAsset
//        ), s"Required 4.5 ${UsdId.toString} as fee for this order, but given 4.49 ${UsdId.toString}")
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
//          tooLowFeeWaves,
//          version
//        ), s"Required 3.75 WAVES as fee for this order, but given 3.7499999 WAVES")
//    }
//  }
//}
