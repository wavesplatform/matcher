package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpV0LevelAgg
import com.wavesplatform.dex.model.LevelAgg

object HttpV0LevelAggConverter {

  def fromLevelAgg(la: LevelAgg): HttpV0LevelAgg = HttpV0LevelAgg(la.amount, la.price)

}
