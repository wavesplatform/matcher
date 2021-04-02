package com.wavesplatform.dex.cli

import cats.syntax.flatMap._
import cats.syntax.option._
import com.typesafe.config.ConfigFactory.parseFile
import com.wavesplatform.dex._
import com.wavesplatform.dex.app.{forceStopApplication, MatcherStateCheckingFailedError}
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.doc.MatcherErrorDoc
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.settings.{loadConfig, MatcherSettings}
import com.wavesplatform.dex.tool.connectors.SuperConnector
import com.wavesplatform.dex.tool.{Checker, ComparisonTool}
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.execution.schedulers.SchedulerService
import pureconfig.ConfigSource
import scopt.{OParser, RenderingMode}
import sttp.client._

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.{Base64, Scanner}
import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success, Try}

object WavesDexCli extends ScoptImplicits {

  implicit val backend = HttpURLConnectionBackend()

  def generateAccountSeed(args: Args): Unit = {
    val seedPromptText = s"Enter the${if (args.accountNonce.isEmpty) " seed of DEX's account" else " base seed"}: "
    val rawSeed = readSeedFromFromStdIn(seedPromptText, args.seedFormat)
    val accountSeed = KeyPair(args.accountNonce.fold(rawSeed)(AccountStorage.getAccountSeed(rawSeed, _)))

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
  }

  def createAccountStorage(args: Args): Unit = {

    val accountFile = args.outputDirectory.toPath.resolve("account.dat").toFile.getAbsoluteFile
    if (accountFile.isFile) {
      System.err.println(s"The '$accountFile' is already exist. If you want to create a file with a new seed, delete the file before.")
      System.exit(1)
    }

    val seedPromptText = s"Enter the${if (args.accountNonce.isEmpty) " seed of DEX's account" else " base seed"}: "
    val rawSeed = readSeedFromFromStdIn(seedPromptText, args.seedFormat)
    val password = readSecretFromStdIn("Enter the password for file: ")
    val accountSeed = args.accountNonce.fold(rawSeed)(AccountStorage.getAccountSeed(rawSeed, _))

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
  }

  def createDocumentation(args: Args): Unit = {
    val outputBasePath = args.outputDirectory.toPath
    val errorsFile = outputBasePath.resolve("errors.md").toFile

    Files.createDirectories(outputBasePath)

    val errors = new PrintWriter(errorsFile)

    try {
      errors.write(MatcherErrorDoc.mkMarkdown)
      println(s"Saved errors documentation to $errorsFile")
    } finally errors.close()
  }

  def createApiKey(args: Args): Unit = {
    val hashedApiKey = Base58.encode(domain.crypto.secureHash(args.apiKey))
    println(s"""Your API Key: $hashedApiKey
               |Don't forget to update your settings:
               |
               |waves.dex.rest-api.api-key-hash = "$hashedApiKey"
               |""".stripMargin)
  }

  def checkServer(args: Args): Unit =
    (
      for {
        _ <- cli.log(
          s"""
             |Passed arguments:
             |  DEX REST API          : ${args.dexRestApi}
             |  Waves Node REST API   : ${args.nodeRestApi}
             |  Expected DEX version  : ${args.version}
             |  DEX config path       : ${args.configPath}
             |  Auth Service REST API : ${args.authServiceRestApi.getOrElse("")}
             |  Account seed          : ${args.accountSeed.getOrElse("")}
                   """.stripMargin
        )
        superConnector <- SuperConnector.create(args.configPath, args.dexRestApi, args.nodeRestApi, args.authServiceRestApi)
        checkResult <- new Checker(superConnector).checkState(args.version, args.accountSeed)
        _ <- cli.lift(superConnector.close())
      } yield checkResult
    ) match {
      case Right(diagnosticNotes) => println(s"$diagnosticNotes\nCongratulations! All checks passed!")
      case Left(error) => println(error); forceStopApplication(MatcherStateCheckingFailedError)
    }

  def runComparison(args: Args): Unit =
    (for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |Running in background
           |""".stripMargin
      )
      tool <- ComparisonTool(args.configPath)
      _ <- cli.lift(tool.run()) // TODO logger context
    } yield ()) match {
      case Right(_) =>
      case Left(error) => println(error); forceStopApplication(MatcherStateCheckingFailedError)
    }

  def makeSnapshots(args: Args): Unit = {
    cli.log(
      s"""
         |Passed arguments:
         |  DEX config path : ${args.configPath}
         |  DEX REST API    : ${args.dexRestApi}
         |  Timeout         : ${args.timeout}
         |Running in background
         |""".stripMargin
    )

    implicit val scheduler: SchedulerService = Scheduler.singleThread(
      name = "time-impl",
      daemonic = false,
      executionModel = ExecutionModel.AlwaysAsyncExecution
    )

    print("Input X-API-KEY: ")
    val key = System.console().readPassword()

    val apiUrl =
      if (args.dexRestApi.isEmpty) {
        val matcherSettings =
          ConfigSource.fromConfig(loadConfig(parseFile(new File(args.configPath)))).at("waves.dex").loadOrThrow[MatcherSettings]
        s"${matcherSettings.restApi.address}:${matcherSettings.restApi.port}"
      } else args.dexRestApi

    def sendRequest(urlPart: String, key: String, method: String = "get"): String = {
      print(s"Sending ${method.toUpperCase} $urlPart... Response: ")
      val r = basicRequest.headers(Map("X-API-KEY" -> key))

      val body = method match {
        case "post" => r.post(uri"$apiUrl/matcher/debug/$urlPart").send().body
        case _ => r.get(uri"$apiUrl/matcher/debug/$urlPart").send().body
      }

      body match {
        case Right(x) => println(x); x
        case Left(e) => println(s"ERROR: $e"); System.exit(1); e
      }
    }

    val currentOffset = sendRequest("currentOffset", key.toString).toInt
    sendRequest("saveSnapshots", key.toString, "post")

    def validate(): Unit = {
      val oldestSnapshotOffset = sendRequest("oldestSnapshotOffset", key.toString).toInt

      if (oldestSnapshotOffset <= currentOffset)
        throw new IllegalArgumentException
      else {
        println(s"Current oldestSnapshotOffset: $oldestSnapshotOffset")
        System.exit(0)
      }
    }

    val validation = Task(validate())
      .onErrorFallbackTo(
        Task(validate())
          .delayExecution(1.second)
          .onErrorRestart(args.timeout.toSeconds)
      )

    Await.ready(validation.runToFuture, args.timeout)

    println("Snapshots wasn't saved before reaching timeout")
    System.exit(1)
  }

  // todo commands:
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
          .action((x, s) => s.copy(addressSchemeByte = x.some)),
        cmd(Command.GenerateAccountSeed.name)
          .action((_, s) => s.copy(command = Command.GenerateAccountSeed.some))
          .text("Generates an account seed from base seed and nonce")
          .children(
            opt[SeedFormat]("seed-format")
              .abbr("sf")
              .text("The format of seed to enter, 'raw-string' by default")
              .valueName("<raw-string,base64,base58>")
              .action((x, s) => s.copy(seedFormat = x)),
            opt[Int]("account-nonce")
              .abbr("an")
              .text("The nonce for account, the default value means you entered the account seed")
              .valueName("<number>")
              .action((x, s) => s.copy(accountNonce = x.some))
          ),
        cmd(Command.CreateAccountStorage.name)
          .action((_, s) => s.copy(command = Command.CreateAccountStorage.some))
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
              .valueName("<raw-string,base64,base58>")
              .action((x, s) => s.copy(seedFormat = x)),
            opt[Int]("account-nonce")
              .abbr("an")
              .text("The nonce for account, the default value means you entered the account seed")
              .valueName("<number>")
              .action((x, s) => s.copy(accountNonce = x.some))
          ),
        cmd(Command.CreateDocumentation.name)
          .action((_, s) => s.copy(command = Command.CreateDocumentation.some))
          .text("Creates a documentation about errors and writes it to the output directory")
          .children(
            opt[File]("output-directory")
              .abbr("od")
              .text("Where to save the documentation")
              .required()
              .action((x, s) => s.copy(outputDirectory = x))
          ),
        cmd(Command.CreateApiKey.name)
          .action((_, s) => s.copy(command = Command.CreateApiKey.some))
          .text("Creates a hashed version of api key and prints settings for DEX server to change it")
          .children(
            opt[String]("api-key")
              .abbr("ak")
              .text("Raw API key, which will be passed to REST API in the X-Api-Key header")
              .required()
              .action((x, s) => s.copy(apiKey = x))
          ),
        cmd(Command.CheckServer.name)
          .action((_, s) => s.copy(command = Command.CheckServer.some))
          .text(s"Checks DEX state")
          .children(
            opt[String]("dex-rest-api")
              .abbr("dra")
              .text("DEX REST API uri. Format: scheme://host:port (default scheme will be picked if none was specified)")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(dexRestApi = x)),
            opt[String]("node-rest-api")
              .abbr("nra")
              .text("Waves Node REST API uri. Format: scheme://host:port (default scheme will be picked if none was specified)")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(nodeRestApi = x)),
            opt[String]("version")
              .abbr("ve")
              .text("DEX expected version")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(version = x)),
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("auth-rest-api")
              .abbr("ara")
              .text("Auth Service REST API uri. Format: scheme://host:port/path/to/token (default scheme will be picked if none was specified)")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(authServiceRestApi = x.some)),
            opt[String]("account-seed")
              .abbr("as")
              .text("Seed for checking account updates")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(accountSeed = x.some))
          ),
        cmd(Command.RunComparison.name)
          .action((_, s) => s.copy(command = Command.RunComparison.some))
          .text("Compares the data from multiple matchers")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x))
          ),
        cmd(Command.MakeOrderbookSnapshots.name)
          .action((_, s) => s.copy(command = Command.MakeOrderbookSnapshots.some))
          .text("Creates snapshots with validating offset after saving")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("dex-rest-api")
              .abbr("dra")
              .text("DEX REST API uri. Format: scheme://host:port (default scheme will be picked if none was specified)")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(dexRestApi = x)),
            opt[FiniteDuration]("timeout")
              .abbr("to")
              .text("Timeout")
              .valueName("<raw-string>")
              .action((x, s) => s.copy(timeout = x))
          )
      )
    }

    // noinspection ScalaStyle
    OParser.parse(parser, rawArgs, Args()).foreach { args =>
      args.command match {
        case None => println(OParser.usage(parser, RenderingMode.TwoColumns))
        case Some(command) =>
          println(s"Running '${command.name}' command")
          AddressScheme.current = new AddressScheme { override val chainId: Byte = args.addressSchemeByte.getOrElse('T').toByte }
          command match {
            case Command.GenerateAccountSeed => generateAccountSeed(args)
            case Command.CreateAccountStorage => createAccountStorage(args)
            case Command.CreateDocumentation => createDocumentation(args)
            case Command.CreateApiKey => createApiKey(args)
            case Command.CheckServer => checkServer(args)
            case Command.RunComparison => runComparison(args)
            case Command.MakeOrderbookSnapshots => makeSnapshots(args)
          }
          println("Done")
      }
    }
  }

  sealed private trait Command {
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

    case object CheckServer extends Command {
      override def name: String = "check-server"
    }

    case object RunComparison extends Command {
      override def name: String = "run-comparison"
    }

    case object MakeOrderbookSnapshots extends Command {
      override def name: String = "make-orderbook-snapshots"
    }

  }

  sealed private trait SeedFormat

  private object SeedFormat {

    case object RawString extends SeedFormat
    case object Base64 extends SeedFormat
    case object Base58 extends SeedFormat

    implicit val seedFormatRead: scopt.Read[SeedFormat] = scopt.Read.reads {
      case "raw-string" => RawString
      case "base64" => Base64
      case "base58" => Base58
      case x => throw new IllegalArgumentException(s"Expected 'raw-string', 'base64' or 'base58', but got '$x'")
    }

  }

  private val defaultFile = new File(".")

  private case class Args(
    addressSchemeByte: Option[Char] = None,
    seedFormat: SeedFormat = SeedFormat.RawString,
    accountNonce: Option[Int] = None,
    command: Option[Command] = None,
    outputDirectory: File = defaultFile,
    apiKey: String = "",
    dexRestApi: String = "",
    nodeRestApi: String = "",
    version: String = "",
    configPath: String = "",
    authServiceRestApi: Option[String] = None,
    accountSeed: Option[String] = None,
    timeout: FiniteDuration = 30 seconds
  )

  // noinspection ScalaStyle
  @scala.annotation.tailrec
  private def readSeedFromFromStdIn(prompt: String, format: SeedFormat): ByteStr = {
    val rawSeed = readSecretFromStdIn(prompt)
    format match {
      case SeedFormat.RawString => rawSeed.getBytes(StandardCharsets.UTF_8)
      case SeedFormat.Base64 =>
        Try(Base64.getDecoder.decode(rawSeed)) match {
          case Success(r) => r
          case Failure(e) =>
            System.err.println(s"Can't parse the seed in the base64 format, try again, $e"); readSeedFromFromStdIn(prompt, format)
        }
      case SeedFormat.Base58 =>
        Base58.tryDecodeWithLimit(rawSeed) match {
          case Success(r) => r
          case Failure(_) => System.err.println("Can't parse the seed in the base58 format, try again"); readSeedFromFromStdIn(prompt, format)
        }
    }
  }

  // noinspection ScalaStyle
  @scala.annotation.tailrec
  private def readSecretFromStdIn(prompt: String): String = {
    val r = Option(System.console) match {
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
