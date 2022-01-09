package com.wavesplatform.dex.settings.utils

import cats.syntax.either._
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.settings.AssetType.lowerCaseNamesToValuesMap
import com.wavesplatform.dex.settings.MatcherSettings.assetPairKeyParser
import pureconfig.ConfigReader
import pureconfig.error.{CannotConvert, FailureReason}
import sttp.model.Uri

trait ConfigReaders {
  val byteStr58ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase58)
  val byteStr64ConfigReader = ConfigReader.fromStringTry(ByteStr.decodeBase64)

  implicit val assetConfigReader = ConfigReader.fromStringOpt(Asset.fromString)
  implicit val issuedAssetConfigReader = byteStr58ConfigReader.map(Asset.IssuedAsset(_))

  implicit val assetPairConfigReader = ConfigReader.fromString(assetPairKeyParser)

  implicit val publicKeyReader = byteStr58ConfigReader.map(PublicKey(_))

  implicit val uriReader = ConfigReader.fromString(x => Uri.parse(x).left.map(e => CannotConvert(x, "Uri", e)))

  implicit val assetTypeConfigReader = ConfigReader.fromString { x =>
    lowerCaseNamesToValuesMap
      .get(x)
      .fold[Either[FailureReason, AssetType]](
        RawFailureReason(s"Unknown asset type: '$x', valid are: ${lowerCaseNamesToValuesMap.values.mkString(", ")}").asLeft
      )(_.asRight)
  }

}

object ConfigReaders extends ConfigReaders
