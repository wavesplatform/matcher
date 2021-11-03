package com.wavesplatform.dex.cli

import cats.Id
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.option._
import com.typesafe.config.ConfigFactory.parseFile
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex._
import com.wavesplatform.dex.app.{forceStopApplication, MatcherStateCheckingFailedError}
import com.wavesplatform.dex.db._
import com.wavesplatform.dex.db.leveldb.{openDb, LevelDb}
import com.wavesplatform.dex.doc.MatcherErrorDoc
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.OrderBookSideSnapshot
import com.wavesplatform.dex.settings.{loadMatcherSettings, MatcherSettings}
import com.wavesplatform.dex.tool._
import com.wavesplatform.dex.tool.connectors.SuperConnector
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{ExecutionModel, Scheduler}
import scopt.{OParser, RenderingMode}
import sttp.client3._

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicLong
import java.util.{Base64, Scanner}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, TimeoutException}
import scala.util.{Failure, Success, Try, Using}

object WavesDexCli extends ScoptImplicits {

  private val backend = HttpURLConnectionBackend()

  // noinspection ScalaStyle
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

  // noinspection ScalaStyle
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

  // noinspection ScalaStyle
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

  // noinspection ScalaStyle
  def createApiKey(args: Args): Unit = {
    val hashedApiKey = Base58.encode(domain.crypto.secureHash(args.apiKey))
    println(s"""Your API Key: $hashedApiKey
               |Don't forget to update your settings:
               |
               |waves.dex.rest-api.api-key-hash = "$hashedApiKey"
               |""".stripMargin)
  }

  // noinspection ScalaStyle
  def checkServer(args: Args, config: Config, matcherSettings: MatcherSettings): Unit = {
    val apiKey = readSecretFromStdIn("Enter X-API-KEY: ")
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
        superConnector <- SuperConnector.create(matcherSettings, args.dexRestApi, args.nodeRestApi, args.authServiceRestApi, apiKey)
        checkResult <- new Checker(superConnector).checkState(args.version, args.accountSeed, config, matcherSettings)
        _ <- cli.lift(superConnector.close())
      } yield checkResult
    ) match {
      case Right(diagnosticNotes) => println(s"$diagnosticNotes\nCongratulations! All checks passed!")
      case Left(error) => println(error); forceStopApplication(MatcherStateCheckingFailedError)
    }
  }

  // noinspection ScalaStyle
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

  // noinspection ScalaStyle
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
      daemonic = true,
      executionModel = ExecutionModel.AlwaysAsyncExecution
    )

    val key = readSecretFromStdIn("Enter X-API-KEY: ")

    def sendRequest(urlPart: String, key: String, method: String = "get"): String = {
      print(s"Sending ${method.toUpperCase} $urlPart... Response: ")
      val r = basicRequest.headers(Map("X-API-KEY" -> key))

      val body = method match {
        case "post" => r.post(uri"${args.dexRestApi}/matcher/debug/$urlPart").send(backend).body
        case _ => r.get(uri"${args.dexRestApi}/matcher/debug/$urlPart").send(backend).body
      }

      body match {
        case Right(x) => println(x); x
        case Left(e) => println(s"ERROR: $e"); System.exit(1); e
      }
    }

    val currentOffset = sendRequest("currentOffset", key).toInt
    sendRequest("saveSnapshots", key, "post")

    val validation = Task(sendRequest("oldestSnapshotOffset", key).toInt <= currentOffset)
      .delayExecution(1.second)
      .onErrorRestart(Long.MaxValue)
      .restartUntil(_ == true)
      .timeout(args.timeout)
      .runToFuture

    Try(Await.ready(validation, args.timeout)) match {
      case Success(_) => println(s"Success!")
      case Failure(e) =>
        e match {
          case _: TimeoutException => println("Snapshots wasn't saved before reaching timeout")
          case _ => println(s"Other error happened:\n${e.printStackTrace()}")
        }
        System.exit(1)
    }
  }

  // noinspection ScalaStyle
  def checkConfig(args: Args): Unit = {
    import PrettyPrinter._

    (for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |""".stripMargin
      )
      result <- ConfigChecker.checkConfig(args.configPath)
    } yield result) match {
      case Right(unused) => prettyPrintUnusedProperties(unused)
      case Left(error) => println(error)
    }
  }

  // noinspection ScalaStyle
  def cleanAssets(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |""".stripMargin
      )
    } yield {
      val count = withLevelDb(matcherSettings.dataDirectory)(cleanAssets)
      println(s"Successfully removed $count assets from LevelDb cache!")
    }

  // noinspection ScalaStyle
  def cleanAssets(levelDb: LevelDb[Id]): Long = levelDb.readWrite[Long] { rw =>
    val removed = new AtomicLong(0)
    rw.iterateOver(DbKeys.AssetPrefix) { entity =>
      rw.delete(entity.getKey)
      removed.incrementAndGet()
    }
    removed.get()
  }

  // noinspection ScalaStyle
  def inspectAsset(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |  Asset id        : ${args.assetId}
           |""".stripMargin
      )
      assetIdBytes <- ByteStr.decodeBase58(args.assetId).toEither
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      AssetsDb.levelDb(db).get(IssuedAsset(assetIdBytes)) match {
        case None => println("There is no such asset")
        case Some(x) =>
          println(
            s"""
               |Decimals   : ${x.decimals}
               |Name       : ${x.name}
               |Has script : ${x.hasScript}
               |Is NFT     : ${x.isNft}
               |""".stripMargin
          )
      }
    }

  // noinspection ScalaStyle
  def setAsset(args: Args, matcherSettings: MatcherSettings): Unit = {
    val name = if (args.name.isEmpty) args.assetId else args.name
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |  Asset id        : ${args.assetId}
           |  Name:           : ${args.name}
           |     Will be used : $name
           |  Decimals        : ${args.decimals}
           |  Has script      : ${args.hasScript}
           |  Is NFT          : ${args.isNft}
           |""".stripMargin
      )
      assetIdBytes <- ByteStr.decodeBase58(args.assetId).toEither
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      val briefAssetDescription = BriefAssetDescription(
        name = name,
        decimals = args.decimals,
        hasScript = args.hasScript,
        isNft = args.isNft
      )

      println(s"Writing $briefAssetDescription...")
      AssetsDb.levelDb(db).put(IssuedAsset(assetIdBytes), briefAssetDescription)
    }
  }

  // noinspection ScalaStyle
  def listAssetPairs(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |""".stripMargin
      )
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      val assetPairs = AssetPairsDb.levelDb(db).all().toVector.sortBy(_.key)
      if (assetPairs.isEmpty) println("There are no asset pairs")
      else {
        println(s"Found ${assetPairs.size} asset pairs:")
        assetPairs.foreach(println)
      }
    }

  // noinspection ScalaStyle
  def inspectOrderBook(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |  Asset pair      : ${args.assetPair}
           |""".stripMargin
      )
      assetPair <- AssetPair.extractAssetPair(args.assetPair).toEither
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      OrderBookSnapshotDb.levelDb(db).get(assetPair) match {
        case None => println("There is no such book")
        case Some((offset, snapshot)) =>
          println(
            s"""
               |Offset     : $offset
               |Last trade : ${snapshot.lastTrade}
               |Asks:
               |${snapshotToStr(snapshot.asks)}
               |Bids:
               |${snapshotToStr(snapshot.bids)}
               |""".stripMargin
          )
      }
    }

  // noinspection ScalaStyle
  def deleteOrderBook(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |  Asset pair      : ${args.assetPair}
           |""".stripMargin
      )
      assetPair <- AssetPair.extractAssetPair(args.assetPair).toEither
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      println("Removing a snapshot...")
      OrderBookSnapshotDb.levelDb(db).delete(assetPair)
      println("Removing from known asset pairs...")
      AssetPairsDb.levelDb(db).remove(assetPair)
    }

  // noinspection ScalaStyle
  def inspectOrder(args: Args, matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |  Order id        : ${args.orderId}
           |""".stripMargin
      )
      oid <- ByteStr.decodeBase58(args.orderId).toEither
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      println("Getting an order...")
      val orderDb = OrderDb.levelDb(matcherSettings.orderDb, db)
      val order = orderDb.get(oid)
      println(order.fold("  not found")(_.jsonStr))
      println("Getting an order info...")
      val orderInfo = orderDb.getOrderInfo(oid)
      println(orderInfo.fold("  not found")(_.toString))
    }

  def copySnapshots(matcherSettings: MatcherSettings): Unit =
    for {
      _ <- cli.log(
        s"""
           |Running copySnapshots:
           |dataDirectory=${matcherSettings.dataDirectory}
           |""".stripMargin
      )
      copyPath = readFromStdIn("Enter path for copy:")
    } yield {
      val f = new File(copyPath)
      if (!f.isDirectory && !f.isFile && !copyPath.contains(matcherSettings.dataDirectory))
        println(s"Copy dir should be empty & should be different than dataDirectory")
      else withLevelDb(matcherSettings.dataDirectory) { originalDb =>
        val originalSnapshotsDb = OrderBookSnapshotDb.levelDb(originalDb)
        val originalAssetPairsDb = AssetPairsDb.levelDb(originalDb)
        withLevelDb(copyPath) { copyDb =>
          val copySnapshotsDb = OrderBookSnapshotDb.levelDb(copyDb)
          originalAssetPairsDb.all().foreach { pair =>
            originalSnapshotsDb.get(pair).foreach { case (offset, snapshot) =>
              copySnapshotsDb.update(pair, offset, snapshot.some)
            }
          }
          println("Finished")
        }
      }
    }

  private def snapshotToStr(snapshot: OrderBookSideSnapshot): String =
    if (snapshot.isEmpty) "empty"
    else snapshot.toVector.sortBy(_._1).map { case (price, os) => s"$price: ${os.mkString(", ")}" }.mkString("  ", "\n  ", "")

  def withLevelDb[T](dataDirectory: String)(f: LevelDb[Id] => T): T =
    Using.resource(openDb(dataDirectory)) { db =>
      f(LevelDb.sync(db))
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
        opt[String]("dex-config")
          .abbr("dc")
          .text("DEX config path")
          .valueName("<raw-string>")
          .action((x, s) => s.copy(configPath = x)),
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
          ),
        cmd(Command.CheckConfigFile.name)
          .action((_, s) => s.copy(command = Command.CheckConfigFile.some))
          .text("Reports all unused properties from file")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x))
          ),
        cmd(Command.CleanAssets.name)
          .action((_, s) => s.copy(command = Command.CleanAssets.some))
          .text("Cleans LevelDb cache with assets")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x))
          ),
        cmd(Command.InspectAsset.name)
          .action((_, s) => s.copy(command = Command.InspectAsset.some))
          .text("Inspect saved information about specified asset")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("asset-id")
              .abbr("aid")
              .text("An asset id")
              .valueName("<asset-id-in-base58>")
              .required()
              .action((x, s) => s.copy(assetId = x))
          ),
        cmd(Command.SetAsset.name)
          .action((_, s) => s.copy(command = Command.SetAsset.some))
          .text("Writes a mock value for this asset. This could be useful when there is asset from the stale fork")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("asset-id")
              .abbr("aid")
              .text("An asset id")
              .valueName("<asset-id-in-base58>")
              .required()
              .action((x, s) => s.copy(assetId = x)),
            opt[String]("name")
              .abbr("n")
              .text("An asset name")
              .valueName("<string>")
              .optional()
              .action((x, s) => s.copy(name = x.trim)),
            opt[Int]("decimals")
              .abbr("d")
              .text("Asset decimals")
              .valueName("<0-8>")
              .optional()
              .validate { x =>
                if (x < 0 || x > 8) Left("Should be in [0; 8]")
                else Right(())
              }
              .action((x, s) => s.copy(decimals = x)),
            opt[Unit]("has-script")
              .abbr("hs")
              .text("This asset has a script")
              .optional()
              .action((x, s) => s.copy(hasScript = true)),
            opt[Unit]("is-nft")
              .abbr("nft")
              .text("This asset is NFT")
              .optional()
              .action((x, s) => s.copy(isNft = true))
          ),
        cmd(Command.ListAssetPairs.name)
          .action((_, s) => s.copy(command = Command.ListAssetPairs.some))
          .text("List known asset pairs from LevelDb")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x))
          ),
        cmd(Command.InspectOrderBook.name)
          .action((_, s) => s.copy(command = Command.InspectOrderBook.some))
          .text("Inspect an order book")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("asset-pair")
              .abbr("ap")
              .text("An asset pair of order book")
              .valueName("<amount-asset-id-in-base58>-<price-asset-id-in-base58>")
              .required()
              .action((x, s) => s.copy(assetPair = x))
          ),
        cmd(Command.DeleteOrderBook.name)
          .action((_, s) => s.copy(command = Command.DeleteOrderBook.some))
          .text("Deletes an order book")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("asset-pair")
              .abbr("ap")
              .text("An asset pair of order book")
              .valueName("<amount-asset-id-in-base58>-<price-asset-id-in-base58>")
              .required()
              .action((x, s) => s.copy(assetPair = x))
          ),
        cmd(Command.InspectOrder.name)
          .action((_, s) => s.copy(command = Command.InspectOrder.some))
          .text("Inspect an order")
          .children(
            opt[String]("dex-config")
              .abbr("dc")
              .text("DEX config path")
              .valueName("<raw-string>")
              .required()
              .action((x, s) => s.copy(configPath = x)),
            opt[String]("order-id")
              .abbr("oid")
              .text("An order id")
              .valueName("<order-id-in-base58>")
              .required()
              .action((x, s) => s.copy(orderId = x))
          )
      )
    }

    def loadAllConfigsUnsafe(path: String): (Config, MatcherSettings) =
      cli.wrapByLogs("  Loading Matcher settings... ") {
        for {
          config <- Try(parseFile(new File(path))).toEither.leftMap(th => s"Cannot load config at path $path: ${th.getWithStackTrace}")
          matcherSettings <- loadMatcherSettings(config)
        } yield config -> matcherSettings
      }.fold(error => throw new RuntimeException(error), identity)

    // noinspection ScalaStyle
    OParser.parse(parser, rawArgs, Args())
      .map { args =>
        args.configPath match {
          case "" => (args, ConfigFactory.empty(), none[MatcherSettings])
          case configPath =>
            lazy val (config, matcherSettings) = loadAllConfigsUnsafe(configPath)
            val updatedArgs = matcherSettings.cli.defaultArgs.coverEmptyValues(args)
            (updatedArgs, config, matcherSettings.some)
        }
      }
      .foreach { case (args, config, mayBeMatcherSettings) =>
        lazy val matcherSettings = mayBeMatcherSettings.getOrElse(throw new RuntimeException("config-path is required"))
        args.command match {
          case None => println(OParser.usage(parser, RenderingMode.TwoColumns))
          case Some(command) =>
            println(s"Running '${command.name}' command")
            AddressScheme.current = new AddressScheme { override val chainId: Byte = args.addressSchemeByte.getOrElse('T').toByte }
            println(s"Current chain id: ${AddressScheme.current.chainId.toChar}")
            command match {
              case Command.GenerateAccountSeed => generateAccountSeed(args)
              case Command.CreateAccountStorage => createAccountStorage(args)
              case Command.CreateDocumentation => createDocumentation(args)
              case Command.CreateApiKey => createApiKey(args)
              case Command.CheckServer => checkServer(args, config, matcherSettings)
              case Command.RunComparison => runComparison(args)
              case Command.MakeOrderbookSnapshots => makeSnapshots(args)
              case Command.CheckConfigFile => checkConfig(args)
              case Command.CleanAssets => cleanAssets(args, matcherSettings)
              case Command.InspectAsset => inspectAsset(args, matcherSettings)
              case Command.SetAsset => setAsset(args, matcherSettings)
              case Command.ListAssetPairs => listAssetPairs(args, matcherSettings)
              case Command.InspectOrderBook => inspectOrderBook(args, matcherSettings)
              case Command.DeleteOrderBook => deleteOrderBook(args, matcherSettings)
              case Command.InspectOrder => inspectOrder(args, matcherSettings)
            }
            println("Done")
        }
      }
  }

  sealed trait Command {
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

    case object CheckConfigFile extends Command {
      override def name: String = "check"
    }

    case object MakeOrderbookSnapshots extends Command {
      override def name: String = "make-orderbook-snapshots"
    }

    case object CleanAssets extends Command {
      override def name: String = "clean-assets"
    }

    case object InspectAsset extends Command {
      override def name: String = "inspect-asset"
    }

    case object SetAsset extends Command {
      override def name: String = "set-asset"
    }

    case object ListAssetPairs extends Command {
      override def name: String = "list-assetpairs"
    }

    case object InspectOrderBook extends Command {
      override def name: String = "inspect-orderbook"
    }

    case object DeleteOrderBook extends Command {
      override def name: String = "delete-orderbook"
    }

    case object InspectOrder extends Command {
      override def name: String = "inspect-order"
    }

  }

  sealed trait SeedFormat

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

  final case class Args(
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
    assetPair: String = "",
    assetId: String = "",
    name: String = "",
    decimals: Int = 8,
    hasScript: Boolean = false,
    isNft: Boolean = false,
    orderId: String = "",
    authServiceRestApi: Option[String] = None,
    accountSeed: Option[String] = None,
    timeout: FiniteDuration = 0 seconds
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
        Base58.tryDecode(rawSeed) match {
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

  private def readFromStdIn(prompt: String): String = {
    System.out.print(prompt)
    val scanner = new Scanner(System.in, StandardCharsets.UTF_8.name())
    if (scanner.hasNextLine) scanner.nextLine() else ""
  }

}
