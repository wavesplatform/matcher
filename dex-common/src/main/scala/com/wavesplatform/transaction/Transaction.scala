package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.{BytesSerializable, JsonSerializable}
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Coeval
import play.api.libs.json.Json

trait Transaction extends BytesSerializable with JsonSerializable {

  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def assetFee: (Asset, Long)
  def timestamp: Long
  def chainByte: Option[Byte] = None

  override def toString: String = json().toString

  def toPrettyString: String = json.map(Json.prettyPrint).value

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets(): Seq[IssuedAsset] = Seq.empty
}
