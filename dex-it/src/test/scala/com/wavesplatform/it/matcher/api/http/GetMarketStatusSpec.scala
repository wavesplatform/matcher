package com.wavesplatform.it.matcher.api.http

import com.softwaremill.sttp.StatusCodes
import com.wavesplatform.dex.api.http.entities.HttpMarketStatus
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.Json

import scala.concurrent.duration.DurationInt

class GetMarketStatusSpec extends MatcherSuiteBase with TableDrivenPropertyChecks {


  private val aliceAssetName = "Alice-X"
  private val IssueResults(issueAliceAssetTx, _, aliceAsset) = mkIssueExtended(alice, aliceAssetName, 1000, 0)

  private val IssueResults(issueBob1Asset1Tx, _, bobAsset1) = mkIssueExtended(bob, "Bob-1-X", someAssetAmount, 5)
  private val IssueResults(issueBob2Asset2Tx, _, bobAsset2) = mkIssueExtended(bob, "Bob-2-X", someAssetAmount, 0)
  private val bob2WavesPair = AssetPair(bobAsset2, Waves)

  private val IssueResults(issueBobNotTradedAssetTx, _, bobNotTradedAsset) = mkIssueExtended(bob, "Bob-Not-Traded", someAssetAmount, 0)
  private val bobNotTradedWavesPair = AssetPair(bobNotTradedAsset, Waves)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  val params =
    "GET /matcher/orderbook/{amountAsset}/{priceAsset}/status " - {

      forAll(Table(
        ("Amount", "Price", "Code", "Is negative", "Error code", "Message"),
        ("WAVES", UsdId.toString, 200, false, 0, "SimpleResponse"),
        ("incorrect", "WAVES", 200, true, 11534345, "The asset incorrect not found"),
        ("WAVES", "incorrect", 404, true, 0, "The WAVES-incorrect asset pair should be reversed")
      )) { (a: String, p: String, c: Int, e: Boolean, mc: Int, m: String) =>

        s"for $a/$p should return (code: $c message: $m) " in {
          val r = dex1.httpApi.orderBookStatus(AssetPair.createAssetPair(a, p).get)

          r.code should be(c)
          r.headers should contain("Content/type", "application/json")

          val b = Json.parse(r.body.left.toString)

          if (e) {
            (b \ "message").as[String] should be equals m
            (b \ "error").as[Int] should be equals mc
          } else (b \ "status").as[String] should be equals m
        }
      }

      "get tickers for unavailable asset should produce error" in {
        dex1.tryApi.orderBookStatus(wctUsdPair) should failWith(11534345, MatcherError.Params(assetId = Some(WctId.toString)))
      }

      "status of empty orderbook" in {
        dex1.api.orderBookInfo(wavesUsdPair).matchingRules.tickSize shouldBe 0.01
      }

      "error of non-existed order" in {
        dex1.httpApi.orderBookInfo(AssetPair(usd, Waves)).code shouldBe StatusCodes.MovedPermanently
      }

      "is updated after trade" in {
        val ask = 5.waves
        val askAmount = 5000000

        val bid = 10.waves
        val bidAmount = 10000000

        dex1.api.place(mkOrder(bob, bob2WavesPair, SELL, askAmount, ask))

        val resp1 = dex1.api.orderBookStatus(bob2WavesPair)
        resp1.lastTrade shouldBe None
        resp1.bestBid shouldBe None
        resp1.bestAsk should matchTo {
          Option(LevelAgg(askAmount, ask))
        }

        dex1.api.place(mkOrder(alice, bob2WavesPair, BUY, bidAmount, bid))

        val resp2 = dex1.api.orderBookStatus(bob2WavesPair)
        resp2.lastTrade should matchTo {
          Option(LastTrade(ask, askAmount, BUY))
        }
        resp2.bestBid should matchTo {
          Option(LevelAgg(bidAmount - askAmount, bid))
        }
        resp2.bestAsk shouldBe None
      }

      "is returned even there is no such order book" in {
        val r = dex1.api.orderBookStatus(bobNotTradedWavesPair)
        r should matchTo(HttpMarketStatus(None, None, None, None, None, None, None))
      }

    }

}
