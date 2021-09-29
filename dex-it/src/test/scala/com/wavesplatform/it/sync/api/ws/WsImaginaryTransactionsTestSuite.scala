package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.entities.{WsAddressFlag, WsTxsData}
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction.Id
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.WsSuiteBase

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration._
import scala.util.{Random, Using}

final class WsImaginaryTransactionsTestSuite extends WsSuiteBase with HasKafka {

  "WsImaginaryTransactionsTestSuite" - {

    "should send 'not' (not observed txs) events" in test { account =>
      Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc1 =>
        Using.resource(mkWsAddressConnection(account, dex1)) { wsc2 =>
          val bid = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 10.usd)
          dex1.api.place(bid)

          val ts = new AtomicLong(System.currentTimeMillis())
          1 to 5 foreach { i =>
            val ask = mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, 10.usd, ts = ts.incrementAndGet())
            placeAndAwaitAtDex(ask, HttpOrderStatus.Status.Filled)
            val txId = ByteStr(dex1.api.getTransactionsByOrderId(ask).head.id().bytes())
            eventually {
              val notObservedTxs = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs)
              withClue(s"it=$i, txId=$txId, askId=${ask.id()}, not=$notObservedTxs") {
                getAggregatedWsTxsData(notObservedTxs) shouldBe Map(txId -> List(ask.id()))
                getAggregatedRemovedTxs(notObservedTxs) shouldBe Seq(txId)
              }
            }
            wsc1.clearMessages()
          }

          wsc2.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs) shouldBe empty
        }
      }
    }

    "should send 'nct' (not created txs) events" in {
      dex2.start()
      test { account =>
        Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc1 =>
          Using.resource(mkWsAddressConnection(account, dex1)) { wsc2 =>
            dex1.blockKafkaTraffic()

            val bid = mkOrder(alice, wavesUsdPair, OrderType.BUY, 5.waves, 10.usd)
            dex2.api.place(bid)

            val ts = new AtomicLong(System.currentTimeMillis())
            val txToOrderId =
              (1 to 5).map { _ =>
                val ask = mkOrder(account, wavesUsdPair, OrderType.SELL, 1.waves, 10.usd, ts = ts.incrementAndGet())
                placeAndAwaitAtDex(ask, HttpOrderStatus.Status.Filled, dex2)
                ByteStr(dex2.api.getTransactionsByOrderId(ask).head.id().bytes()) -> Seq(ask.id())
              }.toMap

            eventually {
              val notCreatedTxs = wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs)
              withClue(s"txToOrderId=$txToOrderId, nct=$notCreatedTxs") {
                getAggregatedWsTxsData(notCreatedTxs) shouldBe txToOrderId
                getAggregatedRemovedTxs(notCreatedTxs) shouldBe empty
              }
            }

            Thread.sleep(5.seconds.toMillis)
            val removedNct = getAggregatedRemovedTxs(wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs))
            removedNct shouldBe empty

            dex1.unblockKafkaTraffic()

            eventually {
              val removedNct = getAggregatedRemovedTxs(wsc1.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs)).sorted
              val txIds = txToOrderId.keys.toSeq.sorted
              withClue(s"txIds=$txIds, removedNct=$removedNct") {
                removedNct shouldBe txIds
              }
            }
            wsc2.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs) shouldBe empty
          }
        }
      }
    }

    "should send tx updates only for exchange transactions" in test { account =>
      Using.resource(mkWsAddressConnection(account, dex1, flags = Set(WsAddressFlag.ImaginaryTxs))) { wsc =>
        broadcastAndAwait(mkTransfer(matcher, account, 1.waves, Waves))
        Thread.sleep(5.seconds.toMillis)
        wsc.collectMessages[WsAddressChanges].flatMap(_.maybeNotObservedTxs) shouldBe empty
        wsc.collectMessages[WsAddressChanges].flatMap(_.maybeNotCreatedTxs) shouldBe empty
      }
    }
  }

  private def getAggregatedWsTxsData(data: List[WsTxsData]): Map[Id, Seq[Id]] =
    data.flatMap(_.txsData).toMap

  private def getAggregatedRemovedTxs(data: List[WsTxsData]): Seq[Id] =
    data.flatMap(_.removedTxs)

  private def test[A](f: KeyPair => A): Unit = {
    val account = mkKeyPair(Random.alphanumeric.take(10).mkString(""))
    broadcastAndAwait(
      mkTransfer(alice, account, 100.waves, Waves),
      mkTransfer(alice, account, 100.usd, usd)
    )
    f(account)
  }

  override protected lazy val dexRunConfig: Config = dexKafkaConfig().withFallback(jwtPublicKeyConfig)

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  web-sockets.external-client-handler.messages-interval = 1ms
         |}""".stripMargin
    )

  private lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    kafka.start()
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

}
