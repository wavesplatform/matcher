package com.wavesplatform.dex.api.http

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.api.JsonSerializer
import com.wavesplatform.dex.model.OrderBookResult

trait TestParsers {
  protected def orderBookFrom(x: HttpResponse): OrderBookResult = JsonSerializer.deserialize[OrderBookResult](
    x.entity
      .asInstanceOf[HttpEntity.Strict]
      .getData()
      .decodeString(StandardCharsets.UTF_8)
  )
}
