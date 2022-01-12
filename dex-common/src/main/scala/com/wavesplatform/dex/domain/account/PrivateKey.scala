package com.wavesplatform.dex.domain.account

import com.wavesplatform.dex.domain.bytes.ByteStr
import play.api.libs.json.{Format, Writes}
import supertagged._

object PrivateKey extends TaggedType[ByteStr] {

  def apply(privateKey: ByteStr): PrivateKey = privateKey @@ PrivateKey
  def apply(privateKey: Array[Byte]): PrivateKey = apply(ByteStr(privateKey))

  def unapply(arg: Array[Byte]): Option[PrivateKey] = Some(apply(arg))

  implicit lazy val jsonFormat: Format[PrivateKey] = Format[PrivateKey](
    ByteStr.byteStrFormat.map(this.apply),
    Writes(pk => ByteStr.byteStrFormat.writes(pk))
  )

}
