package com.wavesplatform.dex

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.{Base64, Scanner}

import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.doc.MatcherErrorDoc
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import scopt.{OParser, RenderingMode}

import scala.util.{Failure, Success, Try}

object WavesDexCli {
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
          .action((x, s) => s.copy(addressSchemeByte = Some(x))),
        cmd(Command.GenerateAccountSeed.name)
          .action((_, s) => s.copy(command = Some(Command.GenerateAccountSeed)))
          .text("Generates an account seed from base seed and nonce")
          .children(
            opt[SeedFormat]("seed-format")
              .abbr("sf")
              .text("The format of seed to enter, 'raw-string' by default")
              .valueName("<raw-string,base64>")
              .action((x, s) => s.copy(seedFormat = x)),
            opt[Int]("account-nonce")
              .abbr("an")
              .text("The nonce for account, the default value means you entered the account seed")
              .valueName("<number>")
              .action((x, s) => s.copy(accountNonce = Some(x)))
          ),
        cmd(Command.CreateAccountStorage.name)
          .action((_, s) => s.copy(command = Some(Command.CreateAccountStorage)))
          .text("Creates an encrypted account storage")
          .children(
            opt[File]("output-directory")
              .abbr("od")
              .text("The directory for a new account.dat file")
              .required()
              .action((x, s) => s.copy(outputDirectory = x)),
            opt[SeedFormat]("seed-format")
              .abbr("sf")
              .text("The format of seed to enter, 'raw-string' by default")
              .valueName("<raw-string,base64>")
              .action((x, s) => s.copy(seedFormat = x)),
            opt[Int]("account-nonce")
              .abbr("an")
              .text("The nonce for account, the default value means you entered the account seed")
              .valueName("<number>")
              .action((x, s) => s.copy(accountNonce = Some(x)))
          ),
        cmd(Command.CreateDocumentation.name)
          .action((_, s) => s.copy(command = Some(Command.CreateDocumentation)))
          .text("Creates a documentation about errors and writes it to the output directory")
          .children(
            opt[File]("output-directory")
              .abbr("od")
              .text("Where to save the documentation")
              .required()
              .action((x, s) => s.copy(outputDirectory = x))
          ),
        cmd(Command.CreateApiKey.name)
          .action((_, s) => s.copy(command = Some(Command.CreateApiKey)))
          .text("Creates a hashed version of api key and prints settings for DEX server to change it")
          .children(
            opt[String]("api-key")
              .abbr("ak")
              .text("Raw API key, which will be passed to REST API in the X-Api-Key header")
              .required()
              .action((x, s) => s.copy(apiKey = x))
          )
      )
    }

    OParser.parse(parser, rawArgs, Args()).foreach { args =>
      args.command match {
        case None => println(OParser.usage(parser, RenderingMode.TwoColumns))
        case Some(command) =>
          println(s"Running '${command.name}' command")
          AddressScheme.current = new AddressScheme {
            override val chainId: Byte = args.addressSchemeByte.getOrElse('T').toByte
          }
          command match {
            case Command.GenerateAccountSeed =>
              val seedPromptText = s"Enter the${if (args.accountNonce.isEmpty) " seed of DEX's account" else " base seed"}: "
              val rawSeed        = readSeedFromFromStdIn(seedPromptText, args.seedFormat)
              val accountSeed    = KeyPair(args.accountNonce.fold(rawSeed)(AccountStorage.getAccountSeed(rawSeed, _)))

              println(s"""Do not share this information with others!
                         |
                         |The seed is:
                         |Base58 format: ${Base58.encode(accountSeed.seed.arr)}
                         |Base64 format: ${Base64.getEncoder.encodeToString(accountSeed.seed.arr)}
                         |
                         |The private key is:
                         |Base58 format: ${Base58.encode(accountSeed.privateKey.arr)}
                         |Base64 format: ${Base64.getEncoder.encodeToString(accountSeed.privateKey.arr)}
                         |
                         |The public key is:
                         |Base58 format: ${Base58.encode(accountSeed.publicKey.arr)}
                         |Base64 format: ${Base64.getEncoder.encodeToString(accountSeed.publicKey.arr)}
                         |
                         |The address is:
                         |Base58 format: ${Base58.encode(accountSeed.publicKey.toAddress.bytes)}
                         |""".stripMargin)

            case Command.CreateAccountStorage =>
              val accountFile = args.outputDirectory.toPath.resolve("account.dat").toFile.getAbsoluteFile
              if (accountFile.isFile) {
                System.err.println(s"The '$accountFile' is already exist. If you want to create a file with a new seed, delete the file before.")
                System.exit(1)
              }

              val seedPromptText = s"Enter the${if (args.accountNonce.isEmpty) " seed of DEX's account" else " base seed"}: "
              val rawSeed        = readSeedFromFromStdIn(seedPromptText, args.seedFormat)
              val password       = readSecretFromStdIn("Enter the password for file: ")
              val accountSeed    = args.accountNonce.fold(rawSeed)(AccountStorage.getAccountSeed(rawSeed, _))
              AccountStorage.save(
                accountSeed,
                AccountStorage.Settings.EncryptedFile(
                  accountFile,
                  password
                )
              )

              println(s"""Saved the seed to '$accountFile'.
                         |Don't forget to update your settings:
                         |
                         |waves.dex {
                         |  account-storage {
                         |    type = "encrypted-file"
                         |    encrypted-file {
                         |      path = "$accountFile"
                         |      password = "paste-entered-password-here"
                         |    }
                         |  }
                         |}
                         |""".stripMargin)

            case Command.CreateDocumentation =>
              val outputBasePath = args.outputDirectory.toPath
              val errorsFile     = outputBasePath.resolve("errors.md").toFile
              Files.createDirectories(outputBasePath)
              val errors = new PrintWriter(errorsFile)
              try {
                errors.write(MatcherErrorDoc.mkMarkdown)
                println(s"Saved errors documentation to $errorsFile")
              } finally {
                errors.close()
              }

            case Command.CreateApiKey =>
              val hashedApiKey = Base58.encode(domain.crypto.secureHash(args.apiKey))
              println(
                s"""Your API Key: $hashedApiKey
                   |Don't forget to update your settings:
                   |
                   |waves.dex.rest-api.api-key-hash = "$hashedApiKey"
                   |""".stripMargin)
          }
          println("Done")
      }
    }
  }

  private sealed trait Command {
    def name: String
  }

  private object Command {
    case object GenerateAccountSeed extends Command {
      override def name: String = "create-account-seed"
    }

    case object CreateAccountStorage extends Command {
      override def name: String = "create-account-storage"
    }

    case object CreateDocumentation extends Command {
      override def name: String = "create-documentation"
    }

    case object CreateApiKey extends Command {
      override def name: String = "create-api-key"
    }
  }

  private sealed trait SeedFormat
  private object SeedFormat {
    case object RawString extends SeedFormat
    case object Base64    extends SeedFormat

    implicit val seedFormatRead: scopt.Read[SeedFormat] = scopt.Read.reads {
      case "raw-string" => RawString
      case "base64"     => Base64
      case x            => throw new IllegalArgumentException(s"Expected 'raw-string' or 'base64', but got '$x'")
    }
  }

  private val defaultFile = new File(".")
  private case class Args(addressSchemeByte: Option[Char] = None,
                          seedFormat: SeedFormat = SeedFormat.RawString,
                          accountNonce: Option[Int] = None,
                          command: Option[Command] = None,
                          outputDirectory: File = defaultFile,
                          apiKey: String = "")

  @scala.annotation.tailrec
  private def readSeedFromFromStdIn(prompt: String, format: SeedFormat): ByteStr = {
    val rawSeed = readSecretFromStdIn(prompt)
    format match {
      case SeedFormat.RawString => rawSeed.getBytes(StandardCharsets.UTF_8)
      case SeedFormat.Base64 =>
        Try(Base64.getDecoder.decode(rawSeed)) match {
          case Success(r) => r
          case Failure(_) =>
            System.err.println("Can't parse the seed in the base64 format, try again")
            readSeedFromFromStdIn(prompt, format)
        }
    }
  }

  @scala.annotation.tailrec
  private def readSecretFromStdIn(prompt: String): String = {
    val r = Option(System.console()) match {
      case Some(console) => new String(console.readPassword(prompt))
      case None =>
        System.out.print(prompt)
        val scanner = new Scanner(System.in, StandardCharsets.UTF_8.name())
        if (scanner.hasNextLine) scanner.nextLine() else ""
    }
    if (r.isEmpty) {
      System.err.println("Please enter a non-empty password")
      readSecretFromStdIn(prompt)
    } else r
  }
}
