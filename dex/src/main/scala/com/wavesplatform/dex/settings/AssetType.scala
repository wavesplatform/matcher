package com.wavesplatform.dex.settings

import cats.implicits.catsSyntaxEitherId
import com.wavesplatform.dex.settings.utils.RawFailureReason
import enumeratum.{Enum, EnumEntry, PlayLowercaseJsonEnum}
import pureconfig.ConfigReader
import pureconfig.error.FailureReason

sealed trait AssetType extends EnumEntry

object AssetType extends Enum[AssetType] with PlayLowercaseJsonEnum[AssetType] {
  override val values = findValues

  case object Amount extends AssetType
  case object Price extends AssetType
  case object Spending extends AssetType
  case object Receiving extends AssetType

  implicit val assetTypeConfigReader = ConfigReader.fromString { x =>
    lowerCaseNamesToValuesMap
      .get(x)
      .fold[Either[FailureReason, AssetType]](
        RawFailureReason(s"Unknown asset type: '$x', valid are: ${lowerCaseNamesToValuesMap.values.mkString(", ")}").asLeft
      )(_.asRight)
  }

}
