package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.order.Order

case class ApiSuccessfulCancel(
    message: Order,
    success: Boolean = true,
    @deprecated(message = "This field is unnecessary", since = "1.2.0") status: String = "OrderAccepted",
)

