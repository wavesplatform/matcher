package com.wavesplatform.dex.api.http.entities

import enumeratum.{Enum, EnumEntry, PlayLowercaseJsonEnum}

sealed trait HttpAssetType extends EnumEntry

object HttpAssetType extends Enum[HttpAssetType] with PlayLowercaseJsonEnum[HttpAssetType] {
  override val values = findValues

  case object Amount extends HttpAssetType

  case object Price extends HttpAssetType

  case object Spending extends HttpAssetType

  case object Receiving extends HttpAssetType

}
