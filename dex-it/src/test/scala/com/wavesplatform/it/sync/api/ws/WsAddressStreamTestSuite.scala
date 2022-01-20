package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.Implicits.releasable
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.{WsAddressFlag, WsBalances, WsMatchTransactionInfo, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsAddressSubscribe, WsError, WsUnsubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{AddressAndPublicKeyAreIncompatible, SubscriptionTokenExpired, SubscriptionsLimitReached}
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder, OrderStatus}
import com.wavesplatform.dex.tool.Using._
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Using

class WsAddressStreamTestSuite extends WsSuiteBase with TableDrivenPropertyChecks {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex {
                    |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
                    |  web-sockets.external-client-handler.subscriptions.max-address-number = 3
                    |}""".stripMargin)
    .withFallback(jwtPublicKeyConfig)
    .withFallback(mkCompositeDynamicFeeSettings(UsdId))

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
    dex1.api.upsertAssetRate(usd, 1)
  }

  override def afterEach(): Unit = dex1.api.cancelAllOrdersWithSig(alice)

  private def mkWsAddressConnection(account: KeyPair): WsConnection = mkWsAddressConnection(account, dex1)

  private def mkWsAddressFilteredConnection(account: KeyPair, filters: Set[WsAddressFlag] = Set.empty): WsConnection =
    mkWsAddressConnection(account, dex1, flags = filters)

  "Address stream should" - {

    "correctly handle rejections" in {
      val fooAddress = mkKeyPair("foo").toAddress
      val barKeyPair = mkKeyPair("bar")

      Using.resource(mkDexWsConnection(dex1)) { wsc =>
        wsc.send(
          WsAddressSubscribe(
            fooAddress,
            WsAddressSubscribe.defaultAuthType,
            mkJwt(barKeyPair)
          )
        )

        val errors = wsc.receiveAtLeastN[WsError](1)
        errors.head should matchTo(
          WsError(
            timestamp = 0L, // ignored
            code = AddressAndPublicKeyAreIncompatible.code,
            message = "Address 3Q6LEwEVJVAomd4BjjjSPydZuNN4vDo3fSs and public key 54gGdY9o2vFgzkSMLXQ7iReTJMPo2XiGdaBQSsG5U3un are incompatible"
          )
        )
      }
    }

    "stop send updates after closing by user and resend after user open it again" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)
      val wsc = mkWsAddressConnection(acc, dex1)
      Using.resource(wsc) { wsc =>
        eventually(wsc.balanceChanges should have size 1)
      }

      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.balanceChanges should have size 1

      Using.resource(mkWsAddressConnection(acc, dex1)) { wsc2 =>
        eventually(wsc2.balanceChanges should have size 1)
      }
    }

    "stop send updates after unsubscribe and receive them again after subscribe" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)

      Using.resource(mkWsAddressConnection(acc, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsAddressChanges](1)
        wsc.clearMessages()

        markup("Unsubscribe")
        wsc.send(WsUnsubscribe(acc))
        broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
        wsc.receiveNoMessages()

        markup("Subscribe")
        wsc.send(WsAddressSubscribe(acc, WsAddressSubscribe.defaultAuthType, mkJwt(acc)))
        wsc.receiveAtLeastN[WsAddressChanges](1)
        wsc.clearMessages()

        markup("Update")
        broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
        wsc.receiveAtLeastN[WsAddressChanges](1)
      }
    }

    "send account updates to authenticated user" - {

      "when account is empty" in {
        val account = mkKeyPair("Test")
        Using.resource(mkWsAddressConnection(account)) { wsac =>
          val addressState = wsac.receiveAtLeastN[WsAddressChanges](1).head
          addressState.address shouldBe account.toAddress
          assertChanges(wsac, squash = false)()()
        }
      }

      "when user places and cancels limit orders" in {

        val acc = mkAccountWithBalance(150.usd -> usd, 10.waves -> Waves)

        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(150.0, 0.0)))()

          val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0)
          val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0, feeAsset = usd, matcherFee = 0.3.usd)

          Seq(bo1, bo2).foreach(placeAndAwaitAtDex(_))

          assertChanges(wsc)(
            Map(usd -> WsBalances(50, 100), Waves -> WsBalances(9.997, 0.003)),
            Map(usd -> WsBalances(39.70, 110.30))
          )(
            WsOrder.fromDomain(LimitOrder(bo1)),
            WsOrder.fromDomain(LimitOrder(bo2))
          )

          cancelAndAwait(acc, bo1)
          assertChanges(wsc, squash = false)(Map(usd -> WsBalances(139.70, 10.30), Waves -> WsBalances(10, 0)))(
            WsOrder.fromOrder(bo1, status = OrderStatus.Cancelled.name.some)
          )

          cancelAndAwait(acc, bo2)
          assertChanges(wsc, squash = false)(Map(usd -> WsBalances(150, 0)))(
            WsOrder.fromOrder(bo2, status = OrderStatus.Cancelled.name.some)
          )
        }
      }

      "when user places market order and it is filled" in {

        val tradableBalance: Map[Asset, Long] = Map(Waves -> 51.003.waves)
        val acc = mkAccountWithBalance(tradableBalance(Waves) -> Waves)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          val smo = mkOrderDP(acc, wavesUsdPair, SELL, 50.waves, 1.0)
          val mo = MarketOrder(smo, tradableBalance.apply _)

          Seq(
            15.waves -> 1.2,
            25.waves -> 1.1,
            40.waves -> 1.0
          ).foreach { case (a, p) => placeAndAwaitAtDex(mkOrderDP(alice, wavesUsdPair, BUY, a, p)) }

          dex1.api.placeMarket(smo)
          waitForOrderAtNode(smo)

          assertChanges(wsc)(
            Map(Waves -> WsBalances(1, 50.003)),
            Map(Waves -> WsBalances(1, 35.0021)),
            Map(Waves -> WsBalances(1, 10.0006)),
            Map(Waves -> WsBalances(1, 0)),
            Map(usd -> WsBalances(18, 0)),
            Map(usd -> WsBalances(45.5, 0)),
            Map(usd -> WsBalances(55.5, 0))
          )(
            WsOrder.fromDomain(mo),
            WsOrder.fromOrder(
              mo.order,
              status = OrderStatus.PartiallyFilled.name.some,
              filledAmount = 15.0.some,
              filledFee = 0.0009.some,
              avgWeighedPrice = 1.2.some,
              totalExecutedPriceAssets = 18.0.some,
              matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.2, 15.0, 18.0))
            ),
            WsOrder.fromOrder(
              mo.order,
              status = OrderStatus.PartiallyFilled.name.some,
              filledAmount = 40.0.some,
              filledFee = 0.0024.some,
              avgWeighedPrice = 1.1375.some,
              totalExecutedPriceAssets = 45.5.some,
              matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.1, 25.0, 27.5))
            ),
            WsOrder.fromOrder(
              mo.order,
              status = OrderStatus.Filled.name.some,
              filledAmount = 50.0.some,
              filledFee = 0.003.some,
              avgWeighedPrice = 1.11.some,
              totalExecutedPriceAssets = 55.5.some,
              matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.0, 10.0, 10.0))
            )
          )
        }
        dex1.api.cancelAllOrdersWithSig(alice)
      }

      "when user's order is fully filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>

          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)))()

          val bo = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)

          placeAndAwaitAtDex(bo)
          placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 1.0))

          assertChanges(wsc)(
            Map(Waves -> WsBalances(9.997, 0.003), usd -> WsBalances(0, 10)),
            Map(Waves -> WsBalances(10, 0), usd -> WsBalances(0, 0)),
            // since balance increasing comes after transaction mining, + 10 - 0.003, Waves balance on Node = 19.997
            Map(Waves -> WsBalances(19.997, 0))
          )(
            WsOrder.fromDomain(LimitOrder(bo)),
            WsOrder.fromOrder(
              bo,
              status = OrderStatus.Filled.name.some,
              filledAmount = 10.0.some,
              filledFee = 0.003.some,
              avgWeighedPrice = 1.0.some,
              totalExecutedPriceAssets = 10.0.some,
              matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.0, 10.0, 10.0))
            )
          )
        }

        dex1.api.cancelAllOrdersWithSig(acc)
      }

      "when user's order is partially filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)))()

          val bo = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)
          val limitOrder = LimitOrder(bo)

          placeAndAwaitAtDex(bo)
          placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 5.waves, 1.0))

          eventually {
            wsc.balanceChanges.squashed should matchTo(
              Map(
                usd -> WsBalances(0, 5),
                Waves -> WsBalances(
                  14.997,
                  0.0015
                ) // since balance increasing comes after transaction mining, + 5 - 0.0015, Waves balance on Node = 14.9985
              )
            )

            wsc.orderChanges.squashed should matchTo(
              Map(
                limitOrder.id -> WsOrder
                  .fromDomain(limitOrder)
                  .copy(
                    id = limitOrder.id,
                    status = OrderStatus.PartiallyFilled.name.some,
                    filledAmount = 5.0.some,
                    filledFee = 0.0015.some,
                    avgWeighedPrice = 1.0.some,
                    totalExecutedPriceAssets = 5.0.some,
                    matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.0, 5.0, 5.0))
                  )
              )
            )
          }
          wsc.clearMessages()

          dex1.api.cancelAllOrdersWithSig(acc)

          eventually {
            wsc.balanceChanges.squashed should matchTo(Map(usd -> WsBalances(5, 0), Waves -> WsBalances(14.9985, 0)))
            wsc.orderChanges.squashed should matchTo(
              Map(limitOrder.id -> WsOrder.fromOrder(bo, status = OrderStatus.Cancelled.name.some))
            )
          }
        }
      }

      "when user make a transfer" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)))()
          broadcastAndAwait(mkTransfer(acc, alice.toAddress, 2.usd, usd, feeAmount = 1.waves))
          assertChanges(wsc)(Map(Waves -> WsBalances(9, 0), usd -> WsBalances(8, 0)))()
        }
      }

      "user issued a new asset after establishing the connection" in {

        val acc = mkAccountWithBalance(10.waves -> Waves)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>

          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0)))()
          val IssueResults(txIssue, _, issuedAsset) = mkIssueExtended(acc, "testAsset", 1000.asset8)
          broadcastAndAwait(txIssue)

          assertChanges(wsc)(
            Map(Waves -> WsBalances(9, 0)),
            Map(issuedAsset -> WsBalances(1000, 0))
          )()
        }
      }

      "NTF asset" - {

        def validateBalances(wsc: WsConnection, wb: WsBalances, a: KeyPair, hasNft: Boolean = true): Unit = {
          def assetBalance(asset: String, balance: Double = 1.0): (Asset, WsBalances) = Asset.fromString(asset).get -> WsBalances(balance, 0)

          val nftMap = if (hasNft) wavesNode1.api.nftAssetsByAddress(a).map(a => assetBalance(a.assetId)) else Map.empty

          assertChanges(wsc)(
            (wavesNode1.api.assetsBalance(a).balances.map(b => assetBalance(b.assetId, b.balance.toDouble))
            ++ Map(Waves -> wb)
            ++ nftMap).toMap
          )()
        }

        "issued via IssueTx" in {

          val acc = mkAccountWithBalance(10.waves -> Waves)

          broadcastAndAwait(mkIssue(acc, "testAssetNT", 1L, 0))

          step("should be in the address stream")
          Using.resource(mkWsAddressConnection(acc)) { wsc =>
            validateBalances(wsc, WsBalances(9, 0), acc)
          }

          step("shouldn't be in the filtered address stream")
          Using.resource(mkWsAddressFilteredConnection(acc, Set(WsAddressFlag.ExcludeNft))) { wsc =>
            validateBalances(wsc, WsBalances(9, 0), acc, hasNft = false)
          }

        }

        "issued via InvokeTx should be in the address stream" in {

          /*
            {-# STDLIB_VERSION 4 #-}
            {-# CONTENT_TYPE DAPP #-}
            {-# SCRIPT_TYPE ACCOUNT #-}

            @Callable(i)
            func default() = {
              let asset = Issue("Asset", "", 1, 0, false, unit, 0)
              let assetId = asset.calculateAssetId()
              [
                asset
              ]
            }

            @Verifier(tx)
            func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
           */

          val dapp = mkAccountWithBalance(100.waves + setScriptFee + smartFee -> Waves)

          val script = "AAIEAAAAAAAAAAQIAhIAAAAAAAAAAAEAAAABaQEAAAAHZGVmYXVsdAAAAAAEAAAABWFzc2V0CQAEQwA" +
            "AAAcCAAAABUFzc2V0AgAAAAAAAAAAAAAAAAEAAAAAAAAAAAAHBQAAAAR1bml0AAAAAAAAAAAABAAAAAdhc3NldElkC" +
            "QAEOAAAAAEFAAAABWFzc2V0CQAETAAAAAIFAAAABWFzc2V0BQAAAANuaWwAAAABAAAAAnR4AQAAAAZ2ZXJpZnkAAAA" +
            "ACQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnByb29mcwAAAAAAAAAAAAgFA" +
            "AAAAnR4AAAAD3NlbmRlclB1YmxpY0tleUzoh4g="

          broadcastAndAwait(mkSetAccountMayBeScript(dapp, Some(Scripts.fromBase64(script)), fee = setScriptFee + smartFee))
          broadcastAndAwait(mkInvokeScript(alice, dapp, fee = 1.005.waves))

          step("should be in the address stream")
          Using.resource(mkWsAddressConnection(dapp)) { wsc =>
            validateBalances(wsc, WsBalances(100, 0), dapp)
          }

          step("shouldn't be in the filtered address stream")
          Using.resource(mkWsAddressFilteredConnection(dapp, Set(WsAddressFlag.ExcludeNft))) { wsc =>
            validateBalances(wsc, WsBalances(100, 0), dapp, hasNft = false)
          }
        }

        "issued via InvokeTx and then transferred by the script should be in the recipient's address stream" in {

          /*
           {-# STDLIB_VERSION 4 #-}
           {-# CONTENT_TYPE DAPP #-}
           {-# SCRIPT_TYPE ACCOUNT #-}

           @Callable(i)
           func default() = {
             let asset = Issue("Asset", "", 1, 0, false, unit, 0)
             let assetId = asset.calculateAssetId()
             [
               asset, ScriptTransfer(i.caller, 1, assetId)
             ]
           }

           @Verifier(tx)
           func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
           */

          val acc = mkAccountWithBalance(10.waves -> Waves)
          val dapp = mkAccountWithBalance(100.waves + setScriptFee + smartFee -> Waves)

          val script = "AAIEAAAAAAAAAAQIAhIAAAAAAAAAAAEAAAABaQEAAAAHZGVmYXVsdAAAAAAEAAAABWF" +
            "zc2V0CQAEQwAAAAcCAAAABUFzc2V0AgAAAAAAAAAAAAAAAAEAAAAAAAAAAAAHBQAAAAR1bml0AAAAA" +
            "AAAAAAABAAAAAdhc3NldElkCQAEOAAAAAEFAAAABWFzc2V0CQAETAAAAAIFAAAABWFzc2V0CQAETAA" +
            "AAAIJAQAAAA5TY3JpcHRUcmFuc2ZlcgAAAAMIBQAAAAFpAAAABmNhbGxlcgAAAAAAAAAAAQUAAAAHY" +
            "XNzZXRJZAUAAAADbmlsAAAAAQAAAAJ0eAEAAAAGdmVyaWZ5AAAAAAkAAfQAAAADCAUAAAACdHgAAAA" +
            "JYm9keUJ5dGVzCQABkQAAAAIIBQAAAAJ0eAAAAAZwcm9vZnMAAAAAAAAAAAAIBQAAAAJ0eAAAAA9zZ" +
            "W5kZXJQdWJsaWNLZXmSxqzL"

          broadcastAndAwait(mkSetAccountMayBeScript(dapp, Some(Scripts.fromBase64(script)), fee = setScriptFee + smartFee))
          broadcastAndAwait(mkInvokeScript(acc, dapp, fee = 1.005.waves))

          step("should be in the address stream")
          Using.resource(mkWsAddressConnection(acc)) { wsc =>
            validateBalances(wsc, WsBalances(8.995, 0), acc)
          }

          step("shouldn't be in the filtered address stream")
          Using.resource(mkWsAddressFilteredConnection(acc, Set(WsAddressFlag.ExcludeNft))) { wsc =>
            validateBalances(wsc, WsBalances(8.995, 0), acc, hasNft = false)
          }
        }

        "issued after connection has been established" in {

          val acc = mkAccountWithBalance(10.waves -> Waves)

          step("should be in the address stream")
          Using.resource(mkWsAddressConnection(acc)) { wsc =>
            broadcast(mkIssue(acc, "testAssetNT", 1L, 0))
            validateBalances(wsc, WsBalances(9, 0), acc)
          }

          step("shouldn't be in the filtered address stream")
          Using.resource(mkWsAddressFilteredConnection(acc, Set(WsAddressFlag.ExcludeNft))) { wsc2 =>
            broadcast(mkIssue(acc, "testAssetNT", 1L, 0))
            validateBalances(wsc2, WsBalances(8, 0), acc, hasNft = false)
          }
        }
      }

      "user issued a new asset before establishing the connection" in {

        val acc = mkAccountWithBalance(10.waves -> Waves)
        val IssueResults(txIssue, _, issuedAsset) = mkIssueExtended(acc, "testAsset", 1000.asset8)

        broadcastAndAwait(txIssue)

        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc)(
            Map(Waves -> WsBalances(9, 0)),
            Map(issuedAsset -> WsBalances(1000, 0))
          )()
        }
      }

      "user burnt part of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)))()
          broadcastAndAwait(mkBurn(acc, usd, 10.usd))

          assertChanges(wsc)(
            Map(Waves -> WsBalances(9, 0)),
            Map(usd -> WsBalances(10, 0))
          )()
        }
      }

      "user burnt all of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        Using.resource(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)))()
          broadcastAndAwait(mkBurn(acc, usd, 20.usd))

          assertChanges(wsc)(
            Map(Waves -> WsBalances(9, 0)),
            Map(usd -> WsBalances(0, 0))
          )()
        }
      }
    }

    "send matchTxInfo on order executions" - {

      "when order partially filled" in {
        val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
        Using(mkWsAddressConnection(acc)) { wsc =>
          assertChanges(wsc, squash = false)(Map(usd -> WsBalances(50, 0), Waves -> WsBalances(100, 0)))()

          val order = mkOrder(acc, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
          placeAndAwaitAtDex(order)

          val firstCounterOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 4.5.waves, 1.usd)
          placeAndAwaitAtDex(firstCounterOrder, Status.Filled)

          eventually(wsc.orderChanges should matchTo(List(
            WsOrder.fromDomain(LimitOrder(order)),
            WsOrder.fromOrder(
              order,
              status = OrderStatus.PartiallyFilled.name.some,
              filledAmount = 4.5.some,
              filledFee = 0.00135.some,
              avgWeighedPrice = 1.0.some,
              totalExecutedPriceAssets = 4.5.some,
              matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1.0, 4.5, 4.5))
            )
          )))
          wsc.clearMessages()

          val secondCounterOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 4.waves, 1.usd)
          placeAndAwaitAtDex(secondCounterOrder, Status.Filled)

          eventually(wsc.orderChanges should matchTo(List(WsOrder.fromOrder(
            order,
            status = OrderStatus.PartiallyFilled.name.some,
            filledAmount = 8.5.some,
            filledFee = 0.00255.some,
            avgWeighedPrice = 1.0.some,
            totalExecutedPriceAssets = 8.5.some,
            matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1, 4.0, 4.0))
          ))))
        }
        dex1.api.cancelAllOrdersWithSig(acc)
      }

      "when order filled" in {
        val acc = mkAccountWithBalance(100.waves -> Waves, 50.usd -> usd)
        Using(mkWsAddressConnection(acc)) { wsc =>
          val order = mkOrder(acc, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
          placeAndAwaitAtDex(order)

          eventually(wsc.orderChanges should matchTo(List(WsOrder.fromDomain(LimitOrder(order)))))
          wsc.clearMessages()

          val counterOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1.usd)
          placeAndAwaitAtDex(counterOrder, Status.Filled)

          eventually(wsc.orderChanges should matchTo(List(WsOrder.fromOrder(
            order,
            status = OrderStatus.Filled.name.some,
            filledAmount = 10.0.some,
            filledFee = 0.003.some,
            avgWeighedPrice = 1.0.some,
            totalExecutedPriceAssets = 10.0.some,
            matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1, 10.0, 10.0))
          ))))
        }
        dex1.api.cancelAllOrdersWithSig(acc)
      }

      "when filling market order" in {
        val acc = mkAccountWithBalance(100.waves -> Waves, 150.usd -> usd)
        Using(mkWsAddressConnection(acc)) { wsc =>
          val aliceOrders = Seq(
            mkOrder(acc, wavesUsdPair, OrderType.SELL, 10.waves, 1.2.usd),
            mkOrder(acc, wavesUsdPair, OrderType.SELL, 20.waves, 1.1.usd),
            mkOrder(acc, wavesUsdPair, OrderType.SELL, 30.waves, 1.3.usd)
          )
          val accountsOrder = mkOrder(acc, wavesUsdPair, OrderType.BUY, 50.waves, 1.3.usd)

          aliceOrders.foreach(dex1.api.place)
          aliceOrders.foreach(order => dex1.api.waitForOrderStatus(order, Status.Accepted))
          dex1.api.placeMarket(accountsOrder)
          dex1.api.waitForOrderStatus(accountsOrder, Status.Filled)

          eventually(wsc.orderChanges.squashed(accountsOrder.id()) should matchTo(WsOrder.fromDomain(MarketOrder(
            accountsOrder,
            Long.MaxValue
          )).copy(
            status = OrderStatus.Filled.name.some,
            filledAmount = 50.0.some,
            filledFee = 0.003.some,
            avgWeighedPrice = 1.2.some,
            totalExecutedPriceAssets = 60.0.some,
            matchInfo = Seq(
              WsMatchTransactionInfo(ByteStr.empty, 0L, 1.1, 20.0, 22.0),
              WsMatchTransactionInfo(ByteStr.empty, 0L, 1.2, 10.0, 12.0),
              WsMatchTransactionInfo(ByteStr.empty, 0L, 1.3, 20.0, 26.0)
            )
          )))
        }
      }

      "when trading with itself" in {
        def copyWithCommonPart(wsOrder: WsOrder): WsOrder = wsOrder.copy(
          status = OrderStatus.Filled.name.some,
          filledAmount = 10.0.some,
          filledFee = 0.003.some,
          avgWeighedPrice = 1.0.some,
          totalExecutedPriceAssets = 10.0.some,
          matchInfo = Seq(WsMatchTransactionInfo(ByteStr.empty, 0L, 1, 10.0, 10.0))
        )

        val acc = mkAccountWithBalance(100.usd -> usd, 50.waves -> Waves)
        Using(mkWsAddressConnection(acc)) { wsc =>
          val order1 = mkOrder(acc, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
          placeAndAwaitAtDex(order1)

          val order2 = mkOrder(acc, wavesUsdPair, OrderType.BUY, 10.waves, 1.usd)
          placeAndAwaitAtDex(order2, Status.Filled)

          eventually {
            val orderChanges = wsc.orderChanges.squashed
            orderChanges(order1.id()) should matchTo(
              copyWithCommonPart(WsOrder.fromDomain(LimitOrder(order1)))
            )
            orderChanges(order2.id()) should matchTo(
              copyWithCommonPart(WsOrder.fromDomain(LimitOrder(order2)))
            )

            orderChanges(order1.id()).matchInfo.head.txId shouldBe orderChanges(order2.id()).matchInfo.head.txId
          }
        }
      }
    }
  }

  "Second connection should get the actual data" in {

    val acc = mkAccountWithBalance(500.usd -> usd, 10.waves -> Waves)
    Using.resource(mkWsAddressConnection(acc, dex1)) { wsc1 =>
      assertChanges(wsc1, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(500, 0)))()

      val now = System.currentTimeMillis()

      val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now)
      val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now + 1)

      Seq(bo1, bo2).foreach { o =>
        placeAndAwaitAtDex(o)
        Thread.sleep(150)
      }

      assertChanges(wsc1)(
        Map(usd -> WsBalances(400, 100), Waves -> WsBalances(9.997, 0.003)),
        Map(usd -> WsBalances(300, 200), Waves -> WsBalances(9.994, 0.006))
      )(
        WsOrder.fromDomain(LimitOrder(bo1)),
        WsOrder.fromDomain(LimitOrder(bo2))
      )

      Using.resource(mkWsAddressConnection(acc, dex1)) { wsc2 =>
        assertChanges(wsc2)(Map(Waves -> WsBalances(9.994, 0.006), usd -> WsBalances(300, 200)))(
          WsOrder.fromDomain(LimitOrder(bo1)),
          WsOrder.fromDomain(LimitOrder(bo2))
        )
      }
    }
    dex1.api.cancelAllOrdersWithSig(acc)
  }

  "Zero balances should not be in initial message" in {
    val acc = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
    Using.resource(mkWsAddressConnection(acc)) { wsc =>
      assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)))()
    }

    broadcastAndAwait(mkBurn(acc, usd, 10.usd))
    Using.resource(mkWsAddressConnection(acc)) { wsc =>
      assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(9, 0)))()
    }

    broadcastAndAwait(mkTransfer(alice, acc, 5.usd, usd, 1.waves))
    Using.resource(mkWsAddressConnection(acc)) { wsc =>
      assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(9, 0), usd -> WsBalances(5, 0)))()
    }

    broadcastAndAwait(mkTransfer(acc, alice, 5.usd, usd, 1.waves))
    Using.resource(mkWsAddressConnection(acc)) { wsc =>
      assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(8, 0)))()
    }
  }

  "Subscription should be cancelled after jwt expiration" in {
    val acc = mkAccountWithBalance(10.waves -> Waves)
    Using.resource(mkWsAddressConnection(acc, dex1, subscriptionLifetime = 3.seconds)) { wsc =>
      wsc.receiveAtLeastN[WsAddressChanges](1) // snapshot
      wsc.receiveAtLeastN[WsError](1).head should matchTo(
        WsError(
          0, // ignored
          SubscriptionTokenExpired.code,
          s"The subscription token for address ${acc.toAddress} expired"
        )
      )

      wsc.isClosed shouldBe false

      Seq(3.seconds, 1.hour).foreach { subscriptionLifetime =>
        val jwt = mkJwt(acc, lifetime = subscriptionLifetime)
        wsc.send(WsAddressSubscribe(acc.toAddress, WsAddressSubscribe.defaultAuthType, jwt))
      }

      wsc.receiveAtLeastN[WsAddressChanges](1) // snapshot
      wsc.receiveNoMessagesOf[WsAddressChanges](3.5.seconds)
    }
  }

  "Connection should close old address subscriptions when address subscriptions limit has been reached" in {
    val wsc = mkDexWsConnection(dex1)

    val carol = mkKeyPair("carol")
    val eve = mkKeyPair("eve")

    Seq(alice, bob, carol, eve, alice).foreach { keyPair =>
      wsc.send(WsAddressSubscribe(keyPair, WsAddressSubscribe.defaultAuthType, mkJwt(keyPair)))
      wsc.receiveAtLeastN[WsAddressChanges](1)
    }

    wsc.receiveAtLeastN[WsError](2) should matchTo {
      List(
        WsError.from(SubscriptionsLimitReached(3, alice.toAddress.toString), 0L),
        WsError.from(SubscriptionsLimitReached(3, bob.toAddress.toString), 0L)
      )
    }
  }

  "should send updates without 2nd step (waves's) signature in jwt" in {
    val acc = mkAccountWithBalance(10.waves -> Waves)

    Using.resource(mkDexWsConnection(dex1)) { wsc =>
      wsc.send(
        WsAddressSubscribe(
          acc,
          WsAddressSubscribe.defaultAuthType,
          mkJwt(mkJwtNotSignedPayload(acc))
        )
      )

      eventually(
        wsc.addressStateChanges.head.isDebug should be(true)
      )
    }

  }

  "Bugs" - {
    "DEX-816 Failure of AddressActor" in {
      dex1.stopWithoutRemove()
      broadcastAndAwait(IssueWctTx)
      dex1.start()

      val wsc = mkDexWsConnection(dex1)
      wsc.send(WsAddressSubscribe(bob, WsAddressSubscribe.defaultAuthType, mkJwt(bob)))

      eventually {
        wsc.receiveAtLeastN[WsAddressChanges](1)
      }
    }

    "DEX-817 Invalid WAVES balance after connection (leasing)" in {
      val bobWavesBalanceBefore = dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair)(Waves)

      dex1.stopWithoutRemove()
      val leaseTx = mkLease(bob, alice, bobWavesBalanceBefore - 0.1.waves, fee = leasingFee)
      broadcastAndAwait(leaseTx)
      dex1.start()

      val wsc = mkDexWsConnection(dex1)
      wsc.send(WsAddressSubscribe(bob, WsAddressSubscribe.defaultAuthType, mkJwt(bob)))

      eventually {
        val balance = wsc.receiveAtLeastN[WsAddressChanges](1).map(_.balances).squashed - btc - wct
        balance should matchTo(
          Map[Asset, WsBalances](
            Waves -> WsBalances(Denormalization.denormalizeAmountAndFee(0.1.waves - leasingFee, 8).toDouble, 0)
          )
        )
      }

      broadcastAndAwait(mkLeaseCancel(bob, leaseTx.id()))
    }

    "DEX-818" - {
      "Connections can affect each other" in {
        Using.Manager.unsafe { use =>
          val wscs = use((1 to 10).map(_ => mkWsAddressConnection(bob)))
          val mainWsc = use(mkWsAddressConnection(bob))

          markup("Multiple orders")
          val now = System.currentTimeMillis()
          val orders = (1 to 50).map { i =>
            mkOrderDP(bob, wavesBtcPair, BUY, 1.waves, 0.00012, ts = now + i)
          }

          Future.traverse(orders)(dex1.asyncApi.place).futureValue
          dex1.api.cancelAllOrdersWithSig(bob)

          wscs.par.foreach(_.close())
          Thread.sleep(3000)
          mainWsc.clearMessages()

          markup("A new order")
          placeAndAwaitAtDex(mkOrderDP(bob, wavesBtcPair, BUY, 2.waves, 0.00029))

          eventually {
            mainWsc.receiveAtLeastN[WsAddressChanges](1)
          }
          mainWsc.clearMessages()
        }
      }

      "Negative balances" in {
        val carol = mkAccountWithBalance(5.waves -> Waves)
        Using.resource(mkWsAddressConnection(carol)) { wsc =>
          val now = System.currentTimeMillis()
          val txs = (1 to 2).map { i =>
            mkTransfer(carol, alice, 5.waves - minFee, Waves, minFee, timestamp = now + i)
          }
          val simulation = Future.traverse(txs)(wavesNode1.asyncApi.broadcast(_))
          simulation.futureValue
          wavesNode1.api.waitForHeightArise()

          wsc.balanceChanges.zipWithIndex.foreach {
            case (changes, i) =>
              changes.foreach {
                case (asset, balance) =>
                  withClue(s"$i: $asset -> $balance: ") {
                    balance.tradable should be >= 0.0
                    balance.reserved should be >= 0.0
                  }
              }
          }
        }
      }
    }

    "DEX-827 Wrong balance" in {
      val btcBalance = 461
      val carol = mkAccountWithBalance(25.waves -> Waves, btcBalance.btc -> btc)
      Using.resource(mkWsAddressConnection(carol)) { wsc =>

        val now = System.currentTimeMillis()
        val order1 = mkOrderDP(carol, wavesBtcPair, BUY, 4.7.waves, 6, matcherFee = 0.003.waves, ts = now + 1)
        val order2 = mkOrderDP(carol, wavesBtcPair, BUY, 4.7.waves, 6, matcherFee = 0.003.waves, ts = now + 2)
        val order3 = mkOrderDP(carol, wavesBtcPair, SELL, 10.waves, 6, matcherFee = 0.003.waves)

        dex1.api.place(order1)
        dex1.api.place(order2)

        placeAndAwaitAtDex(order3, HttpOrderStatus.Status.PartiallyFilled)
        dex1.api.cancelAllOrdersWithSig(carol)

        waitForOrderAtNode(order1)
        waitForOrderAtNode(order2)
        waitForOrderAtNode(order3)

        wavesNode1.api.waitForHeightArise()

        val expectedWavesBalance = 25.0 - 0.003 * 2 - 0.003 * 4.7 * 2 / 10

        wavesNode1.api.balance(carol, Waves) shouldBe expectedWavesBalance.waves
        wavesNode1.api.balance(carol, btc) shouldBe btcBalance.btc

        dex1.api.getTradableBalanceByAssetPairAndAddress(carol, wavesBtcPair) should matchTo(
          Map(
            Waves -> expectedWavesBalance.waves,
            btc -> btcBalance.btc
          )
        )

        wsc.balanceChanges.squashed should matchTo(
          Map(
            Waves -> WsBalances(expectedWavesBalance, 0),
            btc -> WsBalances(btcBalance, 0)
          )
        )
      }
    }

    "DEX-828 Excess data in snapshots" in {

      val initialBalance: (Long, Asset) = 10.waves -> Waves
      val expectedBalanceSnapshot: List[Map[Asset, WsBalances]] = List(Map[Asset, WsBalances](Waves -> WsBalances(10, 0)))

      def getBalanceSnapshot(account: KeyPair): List[Map[Asset, WsBalances]] =
        mkWsAddressConnection(account).receiveAtLeastN[WsAddressChanges](1).map(_.balances)

      // with tradable balance request
      val carol = mkAccountWithBalance(initialBalance)
      dex1.api.getTradableBalanceByAssetPairAndAddress(carol, wavesUsdPair)
      getBalanceSnapshot(carol) should matchTo(expectedBalanceSnapshot)

      // without tradable balance request
      val eve = mkAccountWithBalance(initialBalance)
      getBalanceSnapshot(eve) should matchTo(expectedBalanceSnapshot)
    }

    "DEX-1082 Balances not updated" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)
      Using.resource(mkWsAddressConnection(acc, dex1)) { wsc =>

        eventually(wsc.balanceChanges should have size 1)

        broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
        eventually {
          wsc.balanceChanges.last should matchTo(Map[Asset, WsBalances](
            usd -> WsBalances(2.0, 0.0)
          ))
        }
      }
      Thread.sleep(1000)

      broadcastAndAwait(mkTransfer(acc, alice, 2.usd, usd, feeAmount = 1.waves))
      Thread.sleep(1000)

      Using.resource(mkWsAddressConnection(acc, dex1)) { wsc =>
        eventually {
          wsc.balanceChanges should matchTo(List(Map[Asset, WsBalances](
            Waves -> WsBalances(9.0, 0.0)
          )))
        }

        wsc.clearMessages()
        broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
        eventually {
          wsc.balanceChanges.squashed should matchTo(Map[Asset, WsBalances](
            usd -> WsBalances(2.0, 0.0)
          ))
        }
      }
    }
  }
}
