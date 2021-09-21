package com.wavesplatform.dex.api.ws.protocol

import java.nio.charset.StandardCharsets
import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.WsAddressFlag
import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe._
import com.wavesplatform.dex.domain.account.{Address, PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import pdi.jwt.exceptions.{JwtExpirationException, JwtLengthException}
import pdi.jwt.{JwtAlgorithm, JwtJson, JwtOptions}
import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class WsAddressSubscribe(private val key: Address, authType: String, jwt: String, flags: Set[WsAddressFlag] = Set.empty)
    extends WsClientMessage {
  override val tpe: String = WsAddressSubscribe.tpe

  private def maybeWithDebug(payload: JwtPayload): JwtPayload =
    if (crypto.verify(payload.signature, ByteStr(payload.toSign), payload.publicKey))
      payload
    else
      payload.copy(
        signature = payload.signature,
        publicKey = payload.publicKey,
        networkByte = payload.networkByte,
        clientId = payload.clientId,
        firstTokenExpirationInSeconds = payload.firstTokenExpirationInSeconds,
        activeTokenExpirationInSeconds = payload.activeTokenExpirationInSeconds,
        scope = payload.scope,
        isDebug = true
      )

  def validate(jwtPublicKey: String, networkByte: Byte): Either[MatcherError, JwtPayload] =
    for {
      _ <- Either.cond(supportedAuthTypes.contains(authType), (), error.SubscriptionAuthTypeUnsupported(supportedAuthTypes, authType))
      rawJsonPayload <- JwtJson
        .decodeJson(
          token = jwt,
          key = jwtPublicKey,
          algorithms = JwtAlgorithm.allAsymmetric(),
          options = JwtOptions(signature = true, expiration = true, notBefore = true, leeway = leewayInSeconds)
        )
        .toEither
        .left
        .map(toMatcherError(_, key))
      payload <- rawJsonPayload.validate[JwtPayload].asEither.leftMap(_ => error.JwtPayloadBroken)
      _ <- {
        val given = payload.networkByte.head.toByte
        Either.cond(given == networkByte, (), error.TokenNetworkUnexpected(networkByte, given))
      }
      maybeWithDebugPayload = maybeWithDebug(payload)
      _ <- Either.cond(maybeWithDebugPayload.publicKey.toAddress == key, (), error.AddressAndPublicKeyAreIncompatible(key, maybeWithDebugPayload.publicKey))
    } yield maybeWithDebugPayload

}

object WsAddressSubscribe {

  val tpe = "aus"
  val defaultAuthType = "jwt"
  val supportedAuthTypes = Set(defaultAuthType)
  val leewayInSeconds = 10

  private def wsUnapply(arg: WsAddressSubscribe): Option[(String, Address, String, String, Option[Option[Set[WsAddressFlag]]])] = {
    val flagsOpt = if (arg.flags.isEmpty) None else Some(Some(arg.flags))
    (arg.tpe, arg.key, arg.authType, arg.jwt, flagsOpt).some
  }

  implicit val wsAddressSubscribeFormat: Format[WsAddressSubscribe] = (
    (__ \ "T").format[String] and
      (__ \ "S").format[Address] and
      (__ \ "t").format[String] and
      (__ \ "j").format[String] and
      (__ \ "b").formatNullable((__ \ "f").formatNullable[Set[WsAddressFlag]])
  )(
    (_, key, authType, jwt, flags) => WsAddressSubscribe(key, authType, jwt, flags.flatten.getOrElse(Set.empty)),
    unlift(WsAddressSubscribe.wsUnapply)
  )

  case class JwtPayload(
    signature: ByteStr,
    publicKey: PublicKey,
    networkByte: String,
    clientId: String,
    firstTokenExpirationInSeconds: Long,
    activeTokenExpirationInSeconds: Long,
    scope: List[String],
    isDebug: Boolean = false
  ) {
    def toSign: Array[Byte] = JwtPayload.toSignPrefix ++ s"$networkByte:$clientId:$firstTokenExpirationInSeconds".getBytes(StandardCharsets.UTF_8)

    def signed(privateKey: PrivateKey): JwtPayload = copy(signature = crypto.sign(privateKey, toSign))
  }

  object JwtPayload {
    val toSignPrefix: Array[Byte] = Array[Byte](-1, -1, -1, 1)

    implicit val jwtPayloadFormat: OFormat[JwtPayload] = (
      (__ \ "sig").format[ByteStr] and
        (__ \ "pk").format[PublicKey] and
        (__ \ "nb").format[String] and
        (__ \ "cid").format[String] and
        (__ \ "exp0").format[Long] and
        (__ \ "exp").format[Long] and
        (__ \ "scope").format[List[String]] and
        (__ \ "isDebug").format[Boolean]
    )(JwtPayload.apply, unlift(JwtPayload.unapply))

  }

  def toMatcherError(e: Throwable, address: Address): MatcherError = e match {
    case _: JwtLengthException => error.JwtBroken
    case _: JwtExpirationException => error.SubscriptionTokenExpired(address)
    case _ => error.JwtCommonError(e.getMessage)
  }

}
