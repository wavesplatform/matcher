package com.wavesplatform.dex.api.http.protocol

import com.google.common.primitives.Longs
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.crypto
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json._

case class HttpCancelOrder(@ApiModelProperty(
                             value = "Base58 encoded Sender Public Key",
                             dataType = "string",
                             example = "J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf",
                             required = true
                           ) sender: PublicKey,
                           @ApiModelProperty(
                             value = "Base58 encoded Order ID",
                             dataType = "string",
                             example = "7VEr4T9icqopHWLawGAZ7AQiJbjAcnzXn65ekYvbpwnN"
                           ) orderId: Option[ByteStr],
                           @ApiModelProperty(dataType = "integer") timestamp: Option[Long],
                           @ApiModelProperty(
                             value =
                               """Base58 encoded signature
                                    For single order = Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(sender), bytesOf(orderId)))
                                    For all orders = Base58 encoded Curve25519.sign(senderPrivateKey, concat(bytesOf(sender), bigEndianBytes(timestamp)))""",
                             dataType = "string",
                             example = "65bWzBUbniVuxQLyQdKmjtJ9aJzM6M5tmLfiduq8q59gJXCw4AdqEFb8Ae2ULpve5d4XAWe5Gt34331EjwVSvT9u",
                             required = true
                           ) signature: Array[Byte]) {

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = (orderId, timestamp) match {
    case (Some(oid), _)   => sender ++ oid
    case (None, Some(ts)) => sender ++ Longs.toByteArray(ts)
    case (None, None)     => signature // Signature can't sign itself
  }
  @ApiModelProperty(hidden = true)
  val isSignatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, sender))
}

object HttpCancelOrder {

  implicit val byteArrayFormat: Format[Array[Byte]] = Format(
    {
      case JsString(base58String) => Base58.tryDecodeWithLimit(base58String).fold(_ => JsError("Invalid signature"), b => JsSuccess(b))
      case other                  => JsError(s"Expecting string but got $other")
    },
    b => JsString(Base58.encode(b))
  )

  protected implicit val byteStrWrites: Format[ByteStr] = com.wavesplatform.dex.domain.bytes.ByteStr.byteStrFormat

  implicit val format: OFormat[HttpCancelOrder] = Json.format
}
