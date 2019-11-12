package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.MatcherStatusResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest.prop.TableDrivenPropertyChecks._

class OrderPercentFeeTestSuite(/*data: (Byte, String)* */) extends MatcherSuiteBase {

  var accountCounter = 0

  implicit class DoubleOps(value: Double) {
    val waves, eth: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long        = Normalization.normalizePrice(value, 8, 2)
  }

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

  def getBalance(account: KeyPair, asset: String): Long = {
    if (asset.equals("WAVES"))
      node.accountBalances(account.toAddress.toString)._1
    else node.assetBalance(account.toAddress.toString, asset).balance
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

  def calculateFee(from: Long, percent: Double): Long = {
    (from / 100 * percent).toLong
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

  val data1 = Map(
    1.toByte -> "amount",
   // 1.toByte -> "price",
    2.toByte -> "amount",
   // 2.toByte -> "price",
    3.toByte -> "amount",
   // 3.toByte -> "price",
  )

  data1.foreach {

    case (version, assetType) => {

      val percentFee               = 25
      val feeAsset: Option[String] = if (assetType.equals("amount")) None else Some(UsdId.toString)
      val orderFeeAsset: Asset     = if (assetType.equals("amount")) Waves else IssuedAsset(UsdId)
      def minimalFee(amount: Long, price: Long = 1): Long = {
        if (assetType.equals("amount")) {
          amount / 100 * percentFee
        } else {
          amount * price / 1.waves / 100 * percentFee
        }
      }
      def tooHighFee(amount: Long, price: Long = 1): Long = minimalFee(amount, price) + 1
      def tooLowFee(amount: Long, price: Long = 1): Long  = minimalFee(amount, price) - 1

      val price  = 0.4.usd
      val amount = 150.waves
      val fee    = 37.5.waves

      val config =
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

      docker.restartNode(node, ConfigFactory.parseString(config))

      s"V$version orders (fee asset type: $assetType) & fees processing" - {

        s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
          val accountBuyer  = createAccountWithBalance(60.usd       -> Some(UsdId.toString), fee -> feeAsset)
          val accountSeller = createAccountWithBalance(amount + fee -> feeAsset)

          node.waitOrderProcessed(wavesUsdPair,
                                  node.placeOrder(accountBuyer, wavesUsdPair, BUY, amount, price, fee, version, feeAsset = orderFeeAsset).message.id)
          node.waitOrderProcessed(
            wavesUsdPair,
            node.placeOrder(accountSeller, wavesUsdPair, SELL, amount, price, fee, version, feeAsset = orderFeeAsset).message.id)

          balancesShouldBe(accountBuyer, amount -> "WAVES", 0L     -> UsdId.toString)
          balancesShouldBe(accountSeller, 0L    -> "WAVES", 60.usd -> UsdId.toString)

          reservedBalancesShouldBe(accountBuyer, 0L  -> UsdId.toString)
          reservedBalancesShouldBe(accountSeller, 0L -> "WAVES")
        }

        s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
          val accountBuyer  = createAccountWithBalance(60.usd         -> Some(UsdId.toString), fee -> feeAsset)
          val accountSeller = createAccountWithBalance(30.waves + fee -> feeAsset)

          node.waitOrderProcessed(wavesUsdPair,
                                  node.placeOrder(accountBuyer, wavesUsdPair, BUY, amount, price, fee, version, feeAsset = orderFeeAsset).message.id)
          node.waitOrderProcessed(
            wavesUsdPair,
            node.placeOrder(accountSeller, wavesUsdPair, SELL, 30.waves, price, fee, version, feeAsset = orderFeeAsset).message.id)

          balancesShouldBe(accountBuyer, 30.waves + fee - 7.5.waves -> "WAVES", 48.usd -> UsdId.toString)
          balancesShouldBe(accountSeller, 0L                        -> "WAVES", 12.usd -> UsdId.toString)

          reservedBalancesShouldBe(accountBuyer, 48.usd -> UsdId.toString)
          reservedBalancesShouldBe(accountSeller, 0L    -> "WAVES")

          node.cancelAllOrders(accountBuyer)
          node.cancelAllOrders(accountSeller)
        }

        s"V$version order should be processed if amount less then fee when fee asset-type = $assetType" in {
          val accountBuyer  = createAccountWithBalance(60.usd     -> Some(UsdId.toString), fee -> feeAsset)
          val accountSeller = createAccountWithBalance(1000.waves -> feeAsset)

          node.waitOrderProcessed(wavesUsdPair,
                                  node.placeOrder(accountBuyer, wavesUsdPair, BUY, amount, price, fee, version, feeAsset = orderFeeAsset).message.id)
          node.waitOrderProcessed(
            wavesUsdPair,
            node
              .placeOrder(accountSeller, wavesUsdPair, SELL, amount, price, tooHighFee(amount, price), version, feeAsset = orderFeeAsset)
              .message
              .id)

          balancesShouldBe(accountBuyer, amount                                           -> "WAVES", 0L     -> UsdId.toString)
          balancesShouldBe(accountSeller, 1000.waves - amount - tooHighFee(amount, price) -> "WAVES", 60.usd -> UsdId.toString)
        }

        s"V$version buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
          assertBadRequest(
            node.placeOrder(createAccountWithBalance(price * amount -> Some(UsdId.toString), amount + fee -> feeAsset),
                            wavesUsdPair,
                            BUY,
                            amount,
                            price,
                            tooLowFee(amount, price),
                            version))
        }

        s"V$version sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
          assertBadRequest(
            node.placeOrder(createAccountWithBalance(amount + minFee -> feeAsset),
                            wavesUsdPair,
                            SELL,
                            amount,
                            price,
                            tooLowFee(amount, price),
                            version,
                            feeAsset = orderFeeAsset))
        }

        if (version != 3) {

          s"V$version buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {}

          s"V$version sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {}

        } else {

          s"V$version buy order should be precessed is fee Asset not equal WAVES when fee asset-type = $assetType" in {}

          s"V$version sell order should be precessed is fee Asset not equal WAVES when fee asset-type = $assetType" in {}
        }
      }

    }
  }
}

//class OrdersPercentFeeTestSuite
//    extends OrderPercentFeeTestSuite(
//      1.toByte -> "amount",
//      1.toByte -> "price",
//      2.toByte -> "amount",
//      2.toByte -> "price",
//      3.toByte -> "amount",
//      3.toByte -> "price",
//    ) {}
