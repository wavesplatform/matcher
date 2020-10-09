package com.wavesplatform.dex.load.request

object RequestTag extends Enumeration {
  type RequestTag = Value
  val TRADABLE_BALANCE, ORDER_BOOK_BY_PAIR, ORDER_BOOK_BY_PAIR_AND_KEY, ORDER_STATUS, CANCEL, PLACE = Value
}
