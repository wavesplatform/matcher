package com.wavesplatform.dex.domain.asset

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.{deser, ByteStr}
import com.wavesplatform.dex.domain.validation.Validation
import com.wavesplatform.dex.domain.validation.Validation.booleanOperators
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

@ApiModel(description = """A pair of assets sorted by two rules:
      1. A price asset is chosen by a priority from priceAssets of /matcher/settings;
      2. If both assets are not present among priceAssets, they are sorted lexicographically: price asset bytes < amount asset bytes""")
case class AssetPair(
  @ApiModelProperty(
    value = "Base58 encoded amount asset ID. Waves is used if field isn't specified",
    dataType = "string",
    example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
  ) amountAsset: Asset,
  @ApiModelProperty(
    value = "Base58 encoded price asset ID. Waves is used if field isn't specified",
    dataType = "string",
    example = "DG2xFkPdDwKUoBkzGAhQtLpSGzfXLiCYPEzeKH2Ad24p"
  ) priceAsset: Asset
) {

  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = priceAsset.toString

  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = amountAsset.toString

  def key: String = amountAssetStr + "-" + priceAssetStr

  override def toString: String = key

  def isValid: Validation = (amountAsset != priceAsset) :| "Invalid AssetPair"
  def bytes: Array[Byte] = amountAsset.byteRepr ++ priceAsset.byteRepr

  def reverse: AssetPair = AssetPair(priceAsset, amountAsset)

  def assets: Set[Asset] = Set(amountAsset, priceAsset)
}

object AssetPair {

  def extractAsset(a: String): Try[Asset] = a match {
    case Asset.WavesName => Success(Waves)
    case other => ByteStr.decodeBase58(other).map(IssuedAsset)
  }

  def extractAssetPair(s: String): Try[AssetPair] = s.split('-') match {
    case Array(amtAssetStr, prcAssetStr) =>
      AssetPair.createAssetPair(amtAssetStr, prcAssetStr).recoverWith { case e =>
        Failure(new Exception(s"$s (${e.getMessage})", e))
      }

    case xs => Failure(new Exception(s"$s (incorrect assets count, expected 2 but got ${xs.length})"))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAsset(amountAsset)
      a2 <- extractAsset(priceAsset)
    } yield AssetPair(a1, a2)

  def fromBytes(xs: Array[Byte]): (AssetPair, Int) = {
    val (amount, offset1) = deser.parseByteArrayOption(xs, 0, Asset.AssetIdLength)
    val (price, offset2) = deser.parseByteArrayOption(xs, offset1, Asset.AssetIdLength)
    (
      AssetPair(
        Asset.fromCompatId(amount.map(ByteStr(_))),
        Asset.fromCompatId(price.map(ByteStr(_)))
      ),
      offset2
    )
  }

  implicit val assetPairFormat: OFormat[AssetPair] = (
    (JsPath \ "amountAsset").formatWithDefault[Asset](Waves) and (JsPath \ "priceAsset").formatWithDefault[Asset](Waves)
  )(AssetPair.apply, Function.unlift(AssetPair.unapply))

  val assetPairKeyAsStringFormat: Format[AssetPair] = Format(
    fjs = Reads {
      case JsString(x) => AssetPair.extractAssetPair(x).fold(e => JsError(e.getMessage), JsSuccess(_))
      case x => JsError(JsPath, s"Expected a string, but got ${x.toString().take(10)}...")
    },
    tjs = Writes { x =>
      JsString(x.key)
    }
  )

}
