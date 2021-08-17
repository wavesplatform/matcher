package com.wavesplatform.dex.settings

import cats.syntax.option._
import com.wavesplatform.dex.cli.WavesDexCli.Args

import scala.concurrent.duration.FiniteDuration

final case class CliSettings(
  ignoreUnusedProperties: Seq[String],
  defaultArgs: DefaultArgs
)

final case class DefaultArgs(
  addressSchemeByte: Char,
  dexRestApi: String,
  nodeRestApi: Option[String],
  authServiceRestApi: Option[String],
  timeout: FiniteDuration
) {

  // noinspection ScalaStyle
  def coverEmptyValues(args: Args): Args = {
    val overrideAddressSchemeByte = args.addressSchemeByte.getOrElse(addressSchemeByte)
    val overrideDexRestApi = if (args.dexRestApi.isEmpty) dexRestApi else args.dexRestApi
    val overrideNodeRestApi = if (args.nodeRestApi.isEmpty) nodeRestApi.getOrElse("") else args.nodeRestApi
    val overrideAuthServiceRestApi = if (args.authServiceRestApi.isEmpty) authServiceRestApi else args.authServiceRestApi
    val overrideTimeout = if (args.timeout.length == 0) args.timeout else args.timeout

    args.copy(
      addressSchemeByte = overrideAddressSchemeByte.some,
      dexRestApi = overrideDexRestApi,
      nodeRestApi = overrideNodeRestApi,
      authServiceRestApi = overrideAuthServiceRestApi,
      timeout = overrideTimeout
    )
  }

}
