package com.wavesplatform.dex.settings

import com.wavesplatform.dex.cli.WavesDexCli.Args

import scala.concurrent.duration.FiniteDuration
import scala.util.chaining._

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
      .tap(_.foreach(x => println(s"overriding address scheme byte with [$x]")))
      .orElse(args.addressSchemeByte)
    val overrideDexRestApi = dexRestApi
      .filterNot(_.isEmpty)
      .tap(_.foreach(x => println(s"overriding dex rest api with [$x]")))
      .getOrElse(args.dexRestApi)
    val overrideNodeRestApi = nodeRestApi
      .filterNot(_.isEmpty)
      .tap(_.foreach(x => println(s"overriding node rest api with [$x]")))
      .getOrElse(args.nodeRestApi)
    val overrideAuthServiceRestApi = authServiceRestApi
      .filterNot(_.isEmpty)
      .tap(_.foreach(x => println(s"overriding auth service rest api with [$x]")))
      .orElse(args.authServiceRestApi)
    val overrideTimeout = timeout
      .tap(_.foreach(x => println(s"overriding timeout with [$x]")))
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
