package com.wavesplatform.dex.domain.asset

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.{ByteStr, deser}
import com.wavesplatform.dex.domain.validation.Validation
import com.wavesplatform.dex.domain.validation.Validation.booleanOperators
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{Format, JsObject, Json}

import scala.annotation.meta.field
import scala.util.{Success, Try}

@ApiModel
case class AssetPair(@(ApiModelProperty @field)(
                       value = "Base58 encoded amount asset id",
                       dataType = "string",
                       example = "WAVES"
                     ) amountAsset: Asset,
                     @(ApiModelProperty @field)(
                       value = "Base58 encoded amount price id",
                       dataType = "string",
                       example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"
                     ) priceAsset: Asset) {

  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = priceAsset.toString

  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = amountAsset.toString

  def key: String = amountAssetStr + "-" + priceAssetStr

  override def toString: String = key

  def isValid: Validation = (amountAsset != priceAsset) :| "Invalid AssetPair"
  def bytes: Array[Byte]  = amountAsset.byteRepr ++ priceAsset.byteRepr

  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.maybeBase58Repr,
    "priceAsset"  -> priceAsset.maybeBase58Repr
  )

  def reverse: AssetPair = AssetPair(priceAsset, amountAsset)

  def assets: Set[Asset] = Set(amountAsset, priceAsset)
}

object AssetPair {

  def extractAsset(a: String): Try[Asset] = a match {
    case Asset.WavesName => Success(Waves)
    case other           => ByteStr.decodeBase58(other).map(IssuedAsset)
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAsset(amountAsset)
      a2 <- extractAsset(priceAsset)
    } yield AssetPair(a1, a2)

  def fromBytes(xs: Array[Byte]): AssetPair = {
    val (amount, offset) = deser.parseByteArrayOption(xs, 0, Asset.AssetIdLength)
    val (price, _)       = deser.parseByteArrayOption(xs, offset, Asset.AssetIdLength)
    AssetPair(
      Asset.fromCompatId(amount.map(ByteStr(_))),
      Asset.fromCompatId(price.map(ByteStr(_)))
    )
  }

  implicit val assetPairReader: ValueReader[AssetPair] = { (cfg, path) =>
    val source    = cfg.getString(path)
    val sourceArr = source.split("-")
    val res = sourceArr match {
      case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
      case _                               => throw new Exception(s"$source (incorrect assets count, expected 2 but got ${sourceArr.size})")
    }
    res fold (ex => throw new Exception(s"$source (${ex.getMessage})"), identity)
  }

  implicit val assetPairFormat: Format[AssetPair] = Json.format[AssetPair]
}
