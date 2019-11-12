package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class V1OrderPercentFeePriceTestSuite extends OrderPercentFeePriceTestSuite(3.toByte)

abstract class OrderPercentFeePriceTestSuite(version: Byte, feeAsset: Asset = IssuedAsset(BtcId)) extends MatcherSuiteBase {
  val assetType = "price"

  override protected def nodeConfigs: Seq[Config] = {
    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = 300000
         |    }
         |    percent {
         |      asset-type = $assetType
         |      min-fee = $percentFee
         |    }
         |  }
         |}
       """.stripMargin

//    val orderFeeSettingsStr =
//      s"""
//         |waves.dex {
//         |  allowed-order-versions = [1, 2, 3]
//         |  order-fee {
//         |    mode = dynamic
//         |    dynamic {
//         |      base-fee = 300000
//         |    }
//         |    percent {
//         |      asset-type = amount
//         |      min-fee = 10
//         |    }
//         |    fixed {
//         |      asset = "$EthId"
//         |      min-fee = 10
//         |    }
//         |  }
//         |}
//       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueUsdTx, IssueWctTx, IssueUsdTx, IssueEthTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))
    node.waitForTransaction(node.broadcastTransfer(bob, alice.toAddress.toString, 100.btc, 0.003.waves, Some(BtcId.toString), None).id)
  }

  def printBalances(account: KeyPair): Unit = {
    System.out.println(account.toString)
    System.out.println(s"waves: ${node.accountBalances(account.toAddress.toString)._1} ")
    System.out.println(s"usd: ${node.assetBalance(account.toAddress.toString, BtcId.toString).balance} ")
    System.out.println(s"btc: ${node.assetBalance(account.toAddress.toString, BtcId.toString).balance} ")
    System.out.println(s"waves-r: ${node.reservedBalance(account).getOrElse("WAVES", "0")} ")
    System.out.println(s"usd-r: ${node.reservedBalance(account).getOrElse(BtcId.toString, "0")} ")
  }

  def createAccountWithBalance(balances: (Long, Option[String])*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        if (asset != None)
          assert(
            node.assetBalance(bob.toAddress.toString, asset.get.toString).balance >= balance,
            s"Bob doesn't have enough balance in ${asset.get.toString} to make a transfer"
          )
        node.waitForTransaction(node.broadcastTransfer(alice, account.toAddress.toString, balance, 0.003.waves, asset, None).id)
      }
    }
    account
  }

  def getBalance(account: KeyPair, asset: String): Long = {
    if (asset.equals("WAVES"))
      node.accountBalances(account.toAddress.toString)._1
    else node.assetBalance(account.toAddress.toString, asset).balance
  }

  def balancesShouldBe(account: KeyPair, balances: (Long, String)*): Unit = {
    balances.foreach {
      case (balance, asset) => {
        getBalance(account, asset) should be(balance)
      }
    }
  }

  def reservedBalancesShouldBe(account: KeyPair, balances: (Long, String)*): Unit = {
    balances.foreach {
      case (balance, asset) => {
        node.reservedBalance(account).getOrElse(asset, 0) should be(balance)
      }
    }
  }

  val price          = 0.1.btc
  val fullyAmountBtc = 15.btc
//
//  s"V$version orders (fee asset type: $assetType) & fees processing" - {
//    docker.restartNode(node, ConfigFactory.parseString("waves.dex.order-fee.mode = percent"))
//
//    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
//      val aliceBBefore = getBalance(alice, BtcId.toString)
//      val aliceWBefore = getBalance(alice, "WAVES")
//      val bobBBefore   = getBalance(bob, BtcId.toString)
//      val bobWBefore   = getBalance(bob, "WAVES")
//
//      node.waitOrderProcessed(wavesBtcPair, node.placeOrder(bob, wavesBtcPair, BUY, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(BtcId)).message.id)
//      node.waitOrderProcessed(wavesBtcPair, node.placeOrder(alice, wavesBtcPair, SELL, fullyAmountWaves, price, minimalFee, version, feeAsset = IssuedAsset(BtcId)).message.id)
//
//      balancesShouldBe(bob, bobWBefore + fullyAmountWaves     -> "WAVES", bobBBefore - minimalFee   -> BtcId.toString)
//      balancesShouldBe(alice, aliceWBefore - fullyAmountWaves -> "WAVES", aliceBBefore - minimalFee -> BtcId.toString)
//
//      reservedBalancesShouldBe(bob, 0L   -> BtcId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(alice, 0L -> BtcId.toString, 0L -> "WAVES")
//    }
//
//    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountBtc + minimalFee -> Some(BtcId.toString))
//      val accountSeller = createAccountWithBalance(partiallyAmountWaves        -> None, minimalFee -> Some(BtcId.toString))
//
//      node.waitOrderProcessed(wavesBtcPair, node.placeOrder(accountBuyer, wavesBtcPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
//      node.waitOrderProcessed(wavesBtcPair,
//                              node.placeOrder(accountSeller, wavesBtcPair, SELL, partiallyAmountWaves, price, minimalFee, version).message.id)
//
//      balancesShouldBe(accountBuyer, partiallyAmountWaves - partiallyFeeWaves -> "WAVES", partiallyAmountUsd -> BtcId.toString)
//      balancesShouldBe(accountSeller, 0L                                      -> "WAVES", partiallyAmountUsd -> BtcId.toString)
//
//      reservedBalancesShouldBe(accountBuyer, partiallyAmountUsd -> BtcId.toString, 0L -> "WAVES")
//      reservedBalancesShouldBe(accountSeller, 0L                -> BtcId.toString, 0L -> "WAVES")
//
//      node.cancelAllOrders(accountBuyer)
//      node.cancelAllOrders(accountSeller)
//    }
//
//    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
//      val accountBuyer  = createAccountWithBalance(fullyAmountBtc                          -> Some(BtcId.toString))
//      val accountSeller = createAccountWithBalance(1.waves + fullyAmountWaves + tooHighFee -> None)
//
//      printBalances(accountBuyer)
//      printBalances(accountSeller)
//
//      node.waitOrderProcessed(wavesBtcPair, node.placeOrder(accountBuyer, wavesBtcPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
//      node.waitOrderProcessed(wavesBtcPair,
//                              node
//                                .placeOrder(accountSeller, wavesBtcPair, SELL, fullyAmountWaves, price, tooHighFee, version)
//                                .message
//                                .id)
//
//      printBalances(accountBuyer)
//      printBalances(accountSeller)
//
//      balancesShouldBe(accountBuyer, fullyAmountWaves - minimalFee -> "WAVES", 0L             -> BtcId.toString)
//      balancesShouldBe(accountSeller, 1.waves                      -> "WAVES", fullyAmountBtc -> BtcId.toString)
//    }
//
//    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
//      assertBadRequest(
//        node.placeOrder(
//          createAccountWithBalance(fullyAmountBtc -> Some(BtcId.toString), minimalFee -> None),
//          wavesBtcPair,
//          BUY,
//          fullyAmountWaves,
//          price,
//          tooLowFee,
//          version,
//          feeAsset = feeAsset
//        ))
//    }
//
//    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
//      assertBadRequest(
//        node.placeOrder(createAccountWithBalance(fullyAmountWaves + minimalFee -> None),
//                        wavesBtcPair,
//                        SELL,
//                        fullyAmountWaves,
//                        price,
//                        tooLowFee,
//                        version,
//                        feeAsset = feeAsset))
//    }
//
//    if (version != 3 && assetType.equals("price")) {
//
//      s"buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
//        assertBadRequest(
//          node.placeOrder(
//            createAccountWithBalance(fullyAmountBtc + minimalFee -> Some(BtcId.toString), minimalFee -> None),
//            wavesBtcPair,
//            BUY,
//            fullyAmountWaves,
//            price,
//            minimalFee,
//            version,
//            feeAsset = IssuedAsset(BtcId)
//          ))
//      }
//
//      s"sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
//        assertBadRequest(
//          node.placeOrder(
//            createAccountWithBalance(minimalFee -> Some(BtcId.toString), fullyAmountWaves -> None),
//            wavesBtcPair,
//            SELL,
//            fullyAmountWaves,
//            price,
//            minimalFee,
//            version,
//            feeAsset = IssuedAsset(BtcId)
//          ))
//      }
//    }
//  }
}
