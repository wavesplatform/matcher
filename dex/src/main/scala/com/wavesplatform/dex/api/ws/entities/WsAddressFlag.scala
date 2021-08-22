package com.wavesplatform.dex.api.ws.entities

import play.api.libs.json.{Format, Reads, Writes}

sealed abstract class WsAddressFlag(key: String) extends Product with Serializable {
  val value: String = key
}

object WsAddressFlag {
  case object ExcludeNft extends WsAddressFlag("-nft")

  implicit val format: Format[WsAddressFlag] = Format(
    Reads.StringReads.map {
      case "-nft" => ExcludeNft
      case x => throw new IllegalArgumentException(s"Can't parse '$x' as WsAddressBalancesFilter")
    },
    Writes.StringWrites.contramap(_.value)
  )

}
