package com.wavesplatform.dex

import com.wavesplatform.dex.settings.formatValue
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._

package object json {
  // TODO create a function with f
  // create an implementation with formatValue
  val stringAsDoubleFormat: Format[Double] = Format(
    Reads.StringReads.map(_.toDouble),
    Writes.StringWrites.contramap[Double](formatValue(_))
  )

  def assetMapFormat[V: Format]: Format[Map[Asset, V]] = mapFormat[Asset, V](
    stringifyKey = AssetPair.assetIdStr,
    parseKey = x => AssetPair.extractAssetId(x).fold(_ => JsError(s"Can't parse '$x' as key"), JsSuccess(_))
  )

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K]): Format[Map[K, V]] = {
    val vReads = implicitly[Reads[V]]
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) {
            case (r, (k, v)) =>
              for {
                r <- r
                k <- parseKey(k)
                v <- vReads.reads(v)
              } yield r.updated(k, v)
          }
      },
      tjs = Writes.map[V].contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }
}
