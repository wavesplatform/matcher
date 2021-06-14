package com.wavesplatform.dex.settings

import mouse.any._
import com.wavesplatform.dex.cli.WavesDexCli.Args

import scala.concurrent.duration.FiniteDuration

final case class CliSettings(
  ignoreUnusedProperties: Seq[String],
  argsOverrides: ArgsOverrides
)

final case class ArgsOverrides(
  addressSchemeByte: Option[Char],
  dexRestApi: Option[String],
  nodeRestApi: Option[String],
  authServiceRestApi: Option[String],
  timeout: Option[FiniteDuration]
) {

  // noinspection ScalaStyle
  def updateArgs(args: Args): Args = {
    val overrideAddressSchemeByte = addressSchemeByte
      .unsafeTap(_.foreach(x => println(s"overriding address scheme byte [$x]")))
      .orElse(args.addressSchemeByte)
    val overrideDexRestApi = dexRestApi
      .filterNot(_.isEmpty)
      .unsafeTap(_.foreach(x => println(s"overriding dex rest api [$x]")))
      .getOrElse(args.dexRestApi)
    val overrideNodeRestApi = nodeRestApi
      .filterNot(_.isEmpty)
      .unsafeTap(_.foreach(x => println(s"overriding node rest api [$x]")))
      .getOrElse(args.nodeRestApi)
    val overrideAuthServiceRestApi = authServiceRestApi
      .filterNot(_.isEmpty)
      .unsafeTap(_.foreach(x => println(s"overriding auth service rest api [$x]")))
      .orElse(args.authServiceRestApi)
    val overrideTimeout = timeout
      .unsafeTap(_.foreach(x => println(s"overriding timeout [$x]")))
      .getOrElse(args.timeout)

    args.copy(
      addressSchemeByte = overrideAddressSchemeByte,
      dexRestApi = overrideDexRestApi,
      nodeRestApi = overrideNodeRestApi,
      authServiceRestApi = overrideAuthServiceRestApi,
      timeout = overrideTimeout
    )
  }

}
