package com.wavesplatform.dex.domain.asset

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.base58Length
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json._

import scala.util.Success

sealed trait Asset

object Asset {

  val AssetIdLength: Int       = com.wavesplatform.dex.domain.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  val WavesName = "WAVES"

  final case class IssuedAsset(id: ByteStr) extends Asset { override def toString: String = id.base58 }
  final case object Waves                   extends Asset { override def toString: String = WavesName }

  implicit val assetReads: Reads[IssuedAsset] = Reads {
    case JsString(str) if str.length > AssetIdStringLength => JsError("invalid.feeAssetId")
    case JsString(str) =>
      Base58.tryDecodeWithLimit(str) match {
        case Success(arr) => JsSuccess(IssuedAsset(ByteStr(arr)))
        case _            => JsError("Expected base58-encoded assetId")
      }
    case _ => JsError("Expected base58-encoded assetId")
  }

  implicit val assetWrites: Writes[IssuedAsset] = Writes { asset =>
    JsString(asset.id.base58)
  }

  implicit val assetIdReads: Reads[Asset] = Reads {
    case json: JsString => if (json.value == WavesName) JsSuccess(Waves) else assetReads.reads(json)
    case JsNull         => JsSuccess(Waves)
    case _              => JsError("Expected base58-encoded assetId or null")
  }
  implicit val assetIdWrites: Writes[Asset] = Writes {
    case Waves           => JsString(WavesName)
    case IssuedAsset(id) => JsString(id.base58)
  }

  implicit val assetJsonFormat: Format[IssuedAsset] = Format(assetReads, assetWrites)
  implicit val assetIdJsonFormat: Format[Asset]     = Format(assetIdReads, assetIdWrites)

  implicit val assetReader: ValueReader[Asset] = { (cfg, path) =>
    AssetPair.extractAsset(cfg getString path).fold(ex => throw new Exception(ex.getMessage), identity)
  }

  def fromString(maybeStr: Option[String]): Asset = {
    maybeStr.map(x => IssuedAsset(ByteStr.decodeBase58(x).get)).getOrElse(Waves)
  }

  def fromCompatId(maybeBStr: Option[ByteStr]): Asset = {
    maybeBStr.fold[Asset](Waves)(IssuedAsset)
  }

  implicit class AssetOps(private val ai: Asset) extends AnyVal {

    def byteRepr: Array[Byte] = ai match {
      case Waves           => Array(0: Byte)
      case IssuedAsset(id) => (1: Byte) +: id.arr
    }

    def compatId: Option[ByteStr] = ai match {
      case Waves           => None
      case IssuedAsset(id) => Some(id)
    }

    def maybeBase58Repr: Option[String] = ai match {
      case Waves           => None
      case IssuedAsset(id) => Some(id.base58)
    }

    def fold[A](onWaves: => A)(onAsset: IssuedAsset => A): A = ai match {
      case Waves                  => onWaves
      case asset @ IssuedAsset(_) => onAsset(asset)
    }
  }
}
