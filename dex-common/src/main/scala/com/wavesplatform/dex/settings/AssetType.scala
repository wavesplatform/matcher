package com.wavesplatform.dex.settings

import enumeratum.{Enum, EnumEntry, PlayLowercaseJsonEnum}

sealed trait AssetType extends EnumEntry

object AssetType extends Enum[AssetType] with PlayLowercaseJsonEnum[AssetType] {
  override val values = findValues

  case object Amount extends AssetType
  case object Price extends AssetType
  case object Spending extends AssetType
  case object Receiving extends AssetType

}
