package com.wavesplatform.dex.api.websockets

import java.nio.charset.StandardCharsets
import java.util.Base64
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error

import akka.http.scaladsl.model.ws.TextMessage
import cats.syntax.option._
import cats.syntax.either._
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.MatcherError
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsAddressSubscribe(key: Address, jwt: String) extends WsClientMessage {
  override def toStrictTextMessage: TextMessage.Strict = TextMessage.Strict(Json.toJson(this).toString)
  override val tpe: String                             = WsAddressSubscribe.tpe
}

object WsAddressSubscribe {

  val tpe = "aus"

  def wsUnapply(arg: WsAddressSubscribe): Option[(String, Address, String)] = (arg.tpe, arg.key, arg.jwt).some

  implicit val wsWsAddressSubscribeFormat: Format[WsAddressSubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[Address] and
      (__ \ "t").format[String]
  )(
    (_, key, jwt) => WsAddressSubscribe(key, jwt),
    unlift(WsAddressSubscribe.wsUnapply)
  )

  case class JwtPayload(signature: ByteStr,
                        publicKey: PublicKey,
                        networkByte: String,
                        clientId: String,
                        firstTokenExpirationInSeconds: Long,
                        activeTokenExpirationInSeconds: Long) {
    def toSign: Array[Byte] = JwtPayload.toSignPrefix ++ s"$networkByte:$clientId:$firstTokenExpirationInSeconds".getBytes(StandardCharsets.UTF_8)
  }

  object JwtPayload {
    val toSignPrefix = Array[Byte](-1, -1, -1, 1)

    implicit val jwtPayloadFormat: Format[JwtPayload] = (
      (__ \ "sig").format[ByteStr] and
        (__ \ "pk").format[PublicKey] and
        (__ \ "nb").format[String] and
        (__ \ "cid").format[String] and
        (__ \ "exp0").format[Long] and
        (__ \ "exp").format[Long]
    )(JwtPayload.apply, unlift(JwtPayload.unapply))
  }

  // TODO Add whole JWT validation
  def validate(jwt: String, nowInSeconds: Long): Either[MatcherError, JwtPayload] = {
    val parts = jwt.split('.')
    for {
      rawPayload <- Either.cond(parts.length >= 3, parts(1), error.CanNotParseJwt)
      payload    <- Json.parse(Base64.getDecoder.decode(rawPayload)).validate[JwtPayload].asEither.left.map(_ => error.CanNotParseJwtPayload)
      _ <- {
        if (payload.activeTokenExpirationInSeconds > nowInSeconds) ().asRight
        else error.SubscriptionTokenExpired(payload.activeTokenExpirationInSeconds, nowInSeconds).asLeft
      }
      _ <- {
        if (crypto.verify(payload.signature, ByteStr(payload.toSign), payload.publicKey)) payload.asRight
        else error.InvalidTokenSignature.asLeft
      }
    } yield payload
  }

}
