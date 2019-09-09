// TODO DEX-390
//package com.wavesplatform.it.async
//
//import com.typesafe.config.{Config, ConfigFactory}
//import com.wavesplatform.account.KeyPair
//import com.wavesplatform.common.utils.EitherExt2
//import com.wavesplatform.it._
//import com.wavesplatform.it.api.{OrderStatus, UnexpectedStatusCodeException}
//import com.wavesplatform.it.async.CorrectStatusAfterPlaceTestSuite._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.it.util._
//import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
//import com.wavesplatform.transaction.assets.IssueTransactionV1
//import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
//import com.wavesplatform.transaction.transfer.MassTransferTransaction
//
//import scala.concurrent.duration._
//import scala.concurrent.{Await, Future}
//import scala.util.Random
//
//class CorrectStatusAfterPlaceTestSuite extends NewMatcherSuiteBase {
//  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
//    s"""waves.dex {
//       |  price-assets = ["${issueAsset1Tx.id()}", "${issueAsset2Tx.id()}"]
//       |  rest-order-limit = 100
//       |  events-queue {
//       |    local {
//       |      polling-interval = 1s
//       |      max-elements-per-poll = 100
//       |    }
//       |
//       |    kafka.consumer.buffer-size = 100
//       |  }
//       |}
//       |
//       |akka.kafka.consumer.poll-interval = 1s""".stripMargin
//  )
//
//  private val pairs = Seq(
//    AssetPair(Waves, IssuedAsset(issueAsset1Tx.id())),
//    AssetPair(Waves, IssuedAsset(issueAsset2Tx.id())),
//    AssetPair(IssuedAsset(issueAsset2Tx.id()), IssuedAsset(issueAsset1Tx.id())),
//  )
//
//  private val traders: Seq[KeyPair] = (1 to 10).map(_ => KeyPair(Random.nextString(20).getBytes))
//
//  override protected def beforeAll(): Unit = {
//    super.beforeAll()
//
//    val sendAmount = Long.MaxValue / (traders.size + 1)
//
//    val transferWavesTx =
//      mkMassTransfer(bob, Waves, traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, 100.waves))(collection.breakOut))
//    wavesNode1Api.broadcast(transferWavesTx)
//
//    broadcastAndAwait(issueAssetTxs: _*)
//    val transferAssetsTxs = issueAssetTxs.map { issueTx =>
//      mkMassTransfer(issuer,
//                     IssuedAsset(issueTx.id()),
//                     traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, sendAmount))(collection.breakOut))
//    }
//    broadcastAndAwait(transferAssetsTxs: _*)
//
//    wavesNode1Api.waitForTransaction(transferWavesTx.id())
//  }
//
//  "place orders and check their statuses" in {
//    val ts = System.currentTimeMillis()
//
//    val orders = for {
//      account <- traders
//      pair    <- pairs
//      i       <- 1 to 60
//    } yield mkOrder(account, pair, OrderType.SELL, 100000L, 10000L, ts = ts + i)
//
//    val r = Await.result(Future.traverse(orders.grouped(orders.size / 5))(requests), 5.minutes).flatten
//    r.foreach {
//      case (id, status) => withClue(s"$id")(status should not be OrderStatus.NotFound)
//    }
//  }
//
//  private def request(order: Order): Future[(Order.Id, OrderStatus)] =
//    for {
//      _ <- dex1AsyncApi.place(order).recover {
//        case e: UnexpectedStatusCodeException if e.statusCode == 503 || e.responseBody.contains("has already been placed") => // Acceptable
//      }
//      status <- dex1AsyncApi.orderStatus(order)
//    } yield (order.id(), status.status)
//
//  private def requests(orders: Seq[Order]): Future[Seq[(Order.Id, OrderStatus)]] = Future.traverse(orders)(request)
//}
//
//object CorrectStatusAfterPlaceTestSuite {
//  private val issuer = alice
//
//  private val issueAsset1Tx = IssueTransactionV1
//    .selfSigned(
//      sender = issuer,
//      name = "asset1".getBytes,
//      description = Array.emptyByteArray,
//      quantity = Long.MaxValue,
//      decimals = 0,
//      reissuable = false,
//      fee = 1.waves,
//      timestamp = System.currentTimeMillis()
//    )
//    .explicitGet()
//
//  private val issueAsset2Tx = IssueTransactionV1
//    .selfSigned(
//      sender = issuer,
//      name = "asset2".getBytes,
//      description = Array.emptyByteArray,
//      quantity = Long.MaxValue,
//      decimals = 0,
//      reissuable = false,
//      fee = 1.waves,
//      timestamp = System.currentTimeMillis()
//    )
//    .explicitGet()
//
//  private val issueAssetTxs = List(issueAsset1Tx, issueAsset2Tx)
//}
