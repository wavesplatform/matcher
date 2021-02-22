package com.wavesplatform.dex.settings.utils

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.settings.MatcherSettings.assetPairKeyParser
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import sttp.model.Uri

trait ConfigReaders {
  val byteStr58ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase58)
  val byteStr64ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase64)

  implicit val assetConfigReader = ConfigReader.fromStringOpt(Asset.fromString)
  implicit val issuedAssetConfigReader = byteStr58ConfigReader.map(Asset.IssuedAsset(_))

  implicit val assetPairConfigReader = ConfigReader.fromString(assetPairKeyParser)

  implicit val publicKeyReader = byteStr58ConfigReader.map(PublicKey(_))

  implicit val uriReader = ConfigReader.fromString(x => Uri.parse(x).left.map(e => CannotConvert(x, "Uri", e)))
}

object ConfigReaders extends ConfigReaders
