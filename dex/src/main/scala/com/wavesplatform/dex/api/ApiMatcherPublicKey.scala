package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.json._
import play.api.libs.json.Format

case class ApiMatcherPublicKey(key: PublicKey) extends AnyVal

object ApiMatcherPublicKey {
  implicit val apiMatcherPublicKeyFormat: Format[ApiMatcherPublicKey] = PublicKey.publicKeyJsonFormat.coerce(ApiMatcherPublicKey.apply, _.key)
}
