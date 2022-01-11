package com.wavesplatform.dex.api.http.entities

import enumeratum.{Enum, EnumEntry, PlayLowercaseJsonEnum}

sealed trait HttpAssetType extends EnumEntry

object HttpAssetType extends Enum[HttpAssetType] with PlayLowercaseJsonEnum[HttpAssetType] {
  override val values = findValues

  case object HttpAmount extends HttpAssetType

  case object HttpPrice extends HttpAssetType

  case object HttpSpending extends HttpAssetType

  case object HttpReceiving extends HttpAssetType

}
