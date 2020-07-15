package com.wavesplatform.dex.model

import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.effect._
import com.wavesplatform.dex.error
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.OrderValidator.Result
import com.wavesplatform.dex.settings.{MatcherSettings, loadConfig}
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import net.ceedubs.ficus.Ficus._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class AssetPairBuilderSpec extends AnyFreeSpec with Matchers with MockFactory {

  import AssetPairBuilderSpec._

  private def b(v: String) = ByteStr.decodeBase58(v).get

  def awaitResult[A](result: FutureResult[A]): Result[A] = Await.result(result.value, Duration.Inf)

  private val WAVES = "WAVES"
  private val WUSD  = IssuedAsset(ByteStr.decodeBase58("HyFJ3rrq5m7FxdkWtQXkZrDat1F7LjVVGfpSkUuEXQHj").get)
  private val WBTC  = IssuedAsset(ByteStr.decodeBase58("Fmg13HEHJHuZYbtJq8Da8wifJENq8uBxDuWoP9pVe2Qe").get)
  private val WEUR  = IssuedAsset(ByteStr.decodeBase58("2xnE3EdpqXtFgCP156qt1AbyjpqdZ5jGjWo3CwTawcux").get)
  private val WCNY  = IssuedAsset(ByteStr.decodeBase58("6pmDivReTLikwYqQtJTv6dTcE59knriaodB3AK8T9cF8").get)

  private val Asset1 = mkAssetId(1)
  private val Asset2 = mkAssetId(2)
  private val Asset3 = mkAssetId(3)

  private val predefinedPriceAssets =
    List(
      WBTC,
      WUSD,
      WEUR,
      WCNY,
      IssuedAsset(b("8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS")),
    )

  private val blacklistedAssets = Set(Asset3)

  private val priceAssets = ConfigFactory.parseString(s"""waves.dex {
       |  blacklisted-assets  = [${blacklistedAssets.map(_.id.toString).mkString(",")}]
       |  blacklisted-names   = ["name$$"]
       |  price-assets        = [${predefinedPriceAssets.map(_.id.toString).mkString(",")}]
       |  white-list-only     = no
       |  allowed-asset-pairs = [WAVES-${Asset3.id.toString}]
       |}""".stripMargin)

  private val settings = loadConfig(priceAssets).as[MatcherSettings]("waves.dex")

  private def mkBuilder(knownAssets: (IssuedAsset, Option[BriefAssetDescription])*): AssetPairBuilder = {
    val assetDescription =
      knownAssets.toMap
        .map {
          case (k, Some(x)) => k -> liftValueAsync[BriefAssetDescription](BriefAssetDescription(x.name, x.decimals, hasScript = false))
          case (k, None)    => k -> liftErrorAsync[BriefAssetDescription](error.AssetNotFound(k))
        }
        .withDefault { x =>
          throw new NoSuchElementException(s"Can't find '$x' asset")
        }
    new AssetPairBuilder(settings, assetDescription, blacklistedAssets)
  }

  private val pairs = Table(
    ("amount", "price", "result"),
    (WAVES, WUSD.id.toString, Right(())),
    (WUSD.id.toString, WAVES, Left("AssetPairReversed")),
    (WBTC.id.toString, WEUR.id.toString, Left("AssetPairReversed")),
    (WEUR.id.toString, WBTC.id.toString, Right(())),
    (Asset1.id.toString, WAVES, Right(())),
    (WAVES, Asset1.id.toString, Left("AssetPairReversed")),
    (Asset2.id.toString, Asset1.id.toString, Right(())),
    (Asset1.id.toString, Asset2.id.toString, Left("AssetPairReversed")),
    (Asset1.id.toString, WBTC.id.toString, Right(())),
    (WEUR.id.toString, Asset1.id.toString, Left("AssetPairReversed")),
    (WAVES, Asset3.id.toString, Right(())),
  )

  "AssetPairBuilder" - {
    "correctly ordered and assets IDs are valid" in {
      val assets  = (Asset1 :: Asset2 :: Asset3 :: predefinedPriceAssets).map(_ -> mkAssetDescription())
      val builder = mkBuilder(assets: _*)

      forAll(pairs) {
        case (amountAsset, priceAsset, isValid) =>
          val pair = awaitResult { builder.createAssetPair(amountAsset, priceAsset) }
          isValid match {
            case Right(_) => pair shouldBe Symbol("right")
            case Left(e)  => pair should produce(e)
          }
      }
    }
    "rejects a pair when" - {
      "blacklist" - {
        "contains asset id" in {
          val builder = mkBuilder(Asset3 -> mkAssetDescription())
          awaitResult { builder.validateAssetPair(AssetPair(Asset3, Waves)) } should produce("AmountAssetBlacklisted")
        }
        "matchers asset name" in {
          val builder = mkBuilder(
            Asset1 -> mkAssetDescription(),
            Asset2 -> mkAssetDescription("forbidden Asset name"),
            Asset3 -> mkAssetDescription("name of an asset")
          )

          awaitResult { builder.validateAssetPair(AssetPair(Asset3, Asset1)) } should produce("AmountAssetBlacklisted")
          awaitResult { builder.validateAssetPair(AssetPair(Asset2, Asset1)) } should produce("AmountAssetBlacklisted")
        }
      }
      "asset was not issued" in {
        val builder = mkBuilder(
          Asset1 -> None,
          Asset2 -> mkAssetDescription()
        )

        awaitResult { builder.validateAssetPair(AssetPair(Asset2, Asset1)) } should produce("AssetNotFound")
      }
      "amount and price assets are the same" in {
        awaitResult { mkBuilder().validateAssetPair(AssetPair(WUSD, WUSD)) } should produce("AssetPairSameAsset")
      }
      "pair is not in allowedAssetPairs and whiteListOnly is enabled" in {
        val builder   = new AssetPairBuilder(settings.copy(whiteListOnly = true), x => liftErrorAsync(error.AssetNotFound(x)), blacklistedAssets)
        val assetPair = AssetPair(Waves, WUSD)
        awaitResult { builder.validateAssetPair(assetPair) } should produce("AssetPairIsDenied")
      }
    }
  }
}

object AssetPairBuilderSpec {
  private def mkAssetId(index: Byte): IssuedAsset = IssuedAsset(ByteStr(Array.fill[Byte](32)(index)))
  private def mkAssetDescription(assetName: String = ""): Option[BriefAssetDescription] =
    Some(BriefAssetDescription(name = assetName, decimals = 8, hasScript = false))
}
