package com.wavesplatform.dex.load

import java.io.File

import cats.syntax.option._
import com.wavesplatform.dex.Version
import com.wavesplatform.dex.domain.account.AddressScheme
import scopt.{OParser, RenderingMode}

object WavesDexLoadCli {
  // todo commands:
  // base64
  // get account by seed [and nonce]
  def main(rawArgs: Array[String]): Unit = {
    val builder = OParser.builder[Args]

    val parser = {
      import builder._
      OParser.sequence(
        programName("dex-cli"),
        head("DEX CLI", Version.VersionString),
        opt[Char]("address-scheme")
          .abbr("as")
          .text("The network byte as char. By default it is the testnet: 'T'")
          .valueName("<one char>")
          .action((x, s) => s.copy(addressSchemeByte = x)),
        cmd(Command.CreateFeederFile.name)
          .action((_, s) => s.copy(command = Command.CreateFeederFile.some))
          .text("Creates files for Gatling feeder")
          .children(
            opt[File]("output-directory")
              .abbr("od")
              .text("Where to save files")
              .required()
              .action((x, s) => s.copy(outputDirectory = x))
          )
      )
    }

    OParser.parse(parser, rawArgs, Args()).foreach { args =>
      args.command match {
        case None => println(OParser.usage(parser, RenderingMode.TwoColumns))
        case Some(command) =>
          println(s"Running '${command.name}' command")
          AddressScheme.current = new AddressScheme {
            override val chainId: Byte = args.addressSchemeByte.toByte
          }
          command match {
            case Command.CreateFeederFile => GatlingFeeder.mkFile(args.outputDirectory)
          }
          println("Done")
      }
    }
  }

  private sealed trait Command {
    def name: String
  }

  private object Command {

    case object CreateFeederFile extends Command {
      override def name: String = "create-feeder-file"
    }

  }

  private val defaultFile = new File(".")
  private case class Args(addressSchemeByte: Char = 'T', command: Option[Command] = None, outputDirectory: File = defaultFile)

}