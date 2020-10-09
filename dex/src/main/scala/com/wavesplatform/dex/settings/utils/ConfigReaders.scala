package com.wavesplatform.dex.settings.utils

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.settings.MatcherSettings.assetPairKeyParser
import pureconfig.ConfigReader

trait ConfigReaders {
  val byteStr58ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase58)
  val byteStr64ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase64)

  implicit val assetConfigReader = ConfigReader.fromStringOpt(Asset.fromString)
  implicit val issuedAssetConfigReader = byteStr58ConfigReader.map(Asset.IssuedAsset)

  implicit val assetPairConfigReader = ConfigReader.fromString(assetPairKeyParser)
}

object ConfigReaders extends ConfigReaders
