package com.wavesplatform.dex.domain.asset

import com.google.common.collect.Interners
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.base58Length
import play.api.libs.json._

sealed trait Asset extends Product with Serializable

object Asset {

  val AssetIdLength: Int = com.wavesplatform.dex.domain.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)

  val WavesName = "WAVES"

  // TODO Refactor:
  //  1. toString is a method used only for debugging purposes.
  //     Also for better readability it should contain a class like IssuedAsset(stringHere)
  //  2. Used something else for any logic. E.g. trait Representation[T], where T <: String
  sealed abstract case class IssuedAsset private (id: ByteStr) extends Asset { override def toString: String = id.base58 }

  object IssuedAsset {
    private[this] val interner = Interners.newWeakInterner[IssuedAsset]()
    def apply(id: ByteStr): IssuedAsset = interner.intern(new IssuedAsset(id) {})
  }

  final case object Waves extends Asset { override def toString: String = WavesName }

  implicit val assetFormat: Format[Asset] = Format(
    fjs = Reads {
      case JsNull | JsString("") | JsString(`WavesName`) => JsSuccess(Waves)
      case JsString(s) =>
        if (s.length > AssetIdStringLength) JsError(JsPath, JsonValidationError("error.invalid.asset"))
        else
          Base58
            .tryDecodeWithLimit(s)
            .fold[JsResult[Asset]](
              _ => JsError(JsPath, JsonValidationError("error.incorrect.base58")),
              id => JsSuccess(IssuedAsset(id))
            )
      case _ => JsError(JsPath, JsonValidationError("error.expected.jsstring"))
    },
    tjs = Writes(asset => JsString(asset.toString))
  )

  def fromString(x: String): Option[Asset] =
    if (x == WavesName) Some(Waves)
    else ByteStr.decodeBase58(x).fold(_ => None, xs => Some(IssuedAsset(xs)))

  def fromCompatId(maybeBStr: Option[ByteStr]): Asset = maybeBStr.fold[Asset](Waves)(IssuedAsset(_))

  implicit class AssetOps(private val ai: Asset) extends AnyVal {

    def byteRepr: Array[Byte] = ai match {
      case Waves => Array(0: Byte)
      case IssuedAsset(id) => (1: Byte) +: id.arr
    }

    def compatId: Option[ByteStr] = ai match {
      case Waves => None
      case IssuedAsset(id) => Some(id)
    }

    def maybeBase58Repr: Option[String] = ai match {
      case Waves => None
      case IssuedAsset(id) => Some(id.base58)
    }

    def fold[A](onWaves: => A)(onAsset: IssuedAsset => A): A = ai match {
      case Waves => onWaves
      case asset @ IssuedAsset(_) => onAsset(asset)
    }

  }

}
