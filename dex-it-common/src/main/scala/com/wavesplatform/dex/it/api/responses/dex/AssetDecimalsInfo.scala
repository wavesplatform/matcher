package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class AssetDecimalsInfo(decimals: Byte) extends AnyVal

object AssetDecimalsInfo {
  implicit val format: Format[AssetDecimalsInfo] = Json.format
}
