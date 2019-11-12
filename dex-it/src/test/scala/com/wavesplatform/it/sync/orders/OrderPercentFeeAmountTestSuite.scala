package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class V1OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(1.toByte)
class V2OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(2.toByte)
class V3OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(3.toByte)

abstract class OrderPercentFeeAmountTestSuite(version: Byte) extends MatcherSuiteBase {
  val assetType = "amount"

  override protected def nodeConfigs: Seq[Config] = {
    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = percent
         |    percent {
         |      asset-type = $assetType
         |      min-fee = $percentFee
         |    }
         |  }
         |}
       """.stripMargin

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
  }

  def printBalances(account: KeyPair): Unit = {
    System.out.println(account.toString)
    System.out.println(s"waves: ${node.accountBalances(account.toAddress.toString)._1} ")
    System.out.println(s"usd: ${node.assetBalance(account.toAddress.toString, UsdId.toString).balance} ")
    System.out.println(s"waves-r: ${node.reservedBalance(account).getOrElse("WAVES", "0")} ")
    System.out.println(s"usd-r: ${node.reservedBalance(account).getOrElse(UsdId.toString, "0")} ")
  }

  def createAccountWithBalance(balances: (Long, Option[String])*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        if (asset != None)
          assert(
            node.assetBalance(alice.toAddress.toString, asset.get.toString).balance >= balance,
            s"Alice doesn't have enough balance in ${asset.get.toString} to make a transfer"
          )
        node.waitForTransaction(node.broadcastTransfer(alice, account.toAddress.toString, balance, 0.003.waves, asset, None).id)
      }
    }
    account
  }

  def balancesShouldBe(account: KeyPair, balances: (Long, String)*): Unit = {

    def getBalance(account: KeyPair, asset: String): Long = {
      if (asset.equals("WAVES"))
        node.accountBalances(account.toAddress.toString)._1
      else node.assetBalance(account.toAddress.toString, asset).balance
    }

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

  s"V$version orders (fee asset type: $assetType) & fees processing" - {

    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                     -> Some(UsdId.toString))
      val accountSeller = createAccountWithBalance(fullyAmountWaves + minimalFee -> None)

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
      node.waitOrderProcessed(wavesUsdPair,
                              node.placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, minimalFee, version).message.id)

      balancesShouldBe(accountBuyer, fullyAmountWaves - minimalFee -> "WAVES", 0L             -> UsdId.toString)
      balancesShouldBe(accountSeller, 0L                                -> "WAVES", fullyAmountUsd -> UsdId.toString)

      reservedBalancesShouldBe(accountBuyer, 0L  -> UsdId.toString, 0L -> "WAVES")
      reservedBalancesShouldBe(accountSeller, 0L -> UsdId.toString, 0L -> "WAVES")
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                         -> Some(UsdId.toString))
      val accountSeller = createAccountWithBalance(partiallyAmountWaves + minimalFee -> None)

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
      node.waitOrderProcessed(wavesUsdPair,
                              node.placeOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, minimalFee, version).message.id)

      balancesShouldBe(accountBuyer, partiallyAmountWaves - partiallyFeeWaves -> "WAVES", partiallyAmountUsd -> UsdId.toString)
      balancesShouldBe(accountSeller, 0L                                      -> "WAVES", partiallyAmountUsd -> UsdId.toString)

      reservedBalancesShouldBe(accountBuyer, partiallyAmountUsd -> UsdId.toString, 0L -> "WAVES")
      reservedBalancesShouldBe(accountSeller, 0L                -> UsdId.toString, 0L -> "WAVES")

      node.cancelAllOrders(accountBuyer)
      node.cancelAllOrders(accountSeller)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                          -> Some(UsdId.toString))
      val accountSeller = createAccountWithBalance(1.waves + fullyAmountWaves + tooHighFee -> None)

      printBalances(accountBuyer)
      printBalances(accountSeller)

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version).message.id)
      node.waitOrderProcessed(wavesUsdPair,
                              node
                                .placeOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, tooHighFee, version)
                                .message
                                .id)

      printBalances(accountBuyer)
      printBalances(accountSeller)

      balancesShouldBe(accountBuyer, fullyAmountWaves - minimalFee -> "WAVES", 0L             -> UsdId.toString)
      balancesShouldBe(accountSeller, 1.waves                           -> "WAVES", fullyAmountUsd -> UsdId.toString)
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      assertBadRequest(
        node.placeOrder(
          createAccountWithBalance(fullyAmountUsd -> Some(UsdId.toString), minimalFee -> None),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          tooLowFee,
          version
        ))
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      assertBadRequest(
        node.placeOrder(createAccountWithBalance(fullyAmountWaves + minimalFee -> None),
                        wavesUsdPair,
                        SELL,
                        fullyAmountWaves,
                        price,
                        tooLowFee,
                        version))
    }

    if (version != 3) {

      s"buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
        assertBadRequest(
          node.placeOrder(
            createAccountWithBalance(fullyAmountUsd + minimalFee -> Some(UsdId.toString), minimalFee -> None),
            wavesUsdPair,
            BUY,
            fullyAmountWaves,
            price,
            minimalFee,
            version,
            feeAsset = IssuedAsset(UsdId)
          ))
      }

      s"sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
        assertBadRequest(
          node.placeOrder(
            createAccountWithBalance(minimalFee -> Some(UsdId.toString), fullyAmountWaves -> None),
            wavesUsdPair,
            SELL,
            fullyAmountWaves,
            price,
            minimalFee,
            version,
            feeAsset = IssuedAsset(UsdId)
          ))
      }
    }
  }
}
