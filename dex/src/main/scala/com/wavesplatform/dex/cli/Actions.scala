package com.wavesplatform.dex.cli

import cats.Id
import cats.instances.either._
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.wavesplatform.dex.app.{MatcherStateCheckingFailedError, forceStopApplication}
import com.wavesplatform.dex.cli.WavesDexCli.Args
import com.wavesplatform.dex.db.{AccountStorage, AssetPairsDb, AssetsDb, DbKeys, OrderBookSnapshotDb, OrderDb}
import com.wavesplatform.dex.db.leveldb.{LevelDb, openDb}
import com.wavesplatform.dex.doc.MatcherErrorDoc
import com.wavesplatform.dex.{cli, domain}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.model.{AssetPairBuilder, OrderBookSideSnapshot}
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.tool.{Checker, ComparisonTool, ConfigChecker, PrettyPrinter}
import com.wavesplatform.dex.tool.connectors.SuperConnector
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import monix.execution.schedulers.SchedulerService
import sttp.client3._

import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.{Base64, Scanner}
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.{Await, TimeoutException}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try, Using}

object Actions {

  sealed trait SeedFormat

  private[cli] object SeedFormat {

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

  private val backend = HttpURLConnectionBackend()

  // noinspection ScalaStyle
  def deleteOrderBook(args: Args): Unit = {
    cli.log(
      s"""
         |Passed arguments:
         |  DEX config path : ${args.configPath}
         |  DEX REST API    : ${args.dexRestApi}
         |  Timeout         : ${args.timeout}
         |  Asset Pair      : ${args.assetPair}
         |Running in background
         |""".stripMargin
    )

    implicit val scheduler: SchedulerService = Scheduler.singleThread(
      name = "time-impl",
      daemonic = true,
      executionModel = ExecutionModel.AlwaysAsyncExecution
    )

    val key = readSecretFromStdIn("Enter X-API-KEY: ")

    val currentOffset = sendRequest(s"${args.dexRestApi}/matcher/debug/currentOffset", key).toInt
    sendRequest(s"${args.dexRestApi}/matcher/orderbook/${args.assetPair.replace('-', '/')}", key, "delete")
    sendRequest(s"${args.dexRestApi}/matcher/debug/currentOffset", key, "post")

    val validation = Task(sendRequest(args.dexRestApi,"oldestSnapshotOffset", key).toInt <= currentOffset)
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

    val currentOffset = sendRequest(s"${args.dexRestApi}/matcher/debug/currentOffset", key).toInt
    sendRequest(s"${args.dexRestApi}/matcher/debug/currentOffset", key, "post")

    val validation = Task(sendRequest(args.dexRestApi,"oldestSnapshotOffset", key).toInt <= currentOffset)
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
  def deleteOrderBookFromLevelDb(args: Args, matcherSettings: MatcherSettings): Unit =
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

  def lowestSnapshotsOffset(args: Args, matcherSettings: MatcherSettings): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    for {
      _ <- cli.log(
        s"""
           |Passed arguments:
           |  DEX config path : ${args.configPath}
           |""".stripMargin
      )
    } yield withLevelDb(matcherSettings.dataDirectory) { db =>
      val apdb = AssetPairsDb.levelDb(db)
      val obdb = OrderBookSnapshotDb.levelDb(db)
      val apb = new AssetPairBuilder(matcherSettings, null, matcherSettings.blacklistedAssets)

      val pairs = apdb.all()
      val validPairs = pairs.flatMap(apb.quickValidateAssetPair(_).toOption)
      val snapshots = obdb.iterateSnapshots(validPairs.contains)
      val offsets = obdb.iterateOffsets(validPairs.contains)
      val result = validPairs.map { pair =>
        pair -> offsets.get(pair).zip(snapshots.get(pair))
      }.toMap
      val lowestOffset = result.flatMap(x => x._2.map(s => x._1 -> s._1)).toList.sortBy(_._2).headOption
      println(s"Lowest offset: $lowestOffset")
    }
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

  // noinspection ScalaStyle
  def generateFeeSettings(args: Args): Unit = {
    val pairs = for {
      aa <- args.amountAssets
      pa <- args.priceAssets
    } yield s"$aa-$pa"

    val config = pairs.foldLeft(ConfigFactory.empty()) { (cfg, pair) =>
      cfg.withFallback(ConfigFactory.parseString(
        s"""
           |$pair: {
           |  mode = percent
           |  percent {
           |    asset-type = spending
           |    min-fee = ${args.minFee}
           |    min-fee-in-waves = ${args.minFeeInWaves}
           |  }
           |}
           |""".stripMargin
      ))
    }

    val options = ConfigRenderOptions
      .defaults()
      .setComments(false)
      .setOriginComments(false)
      .setFormatted(true)
      .setJson(false)

    print(config.root().render(options))
  }

  private def snapshotToStr(snapshot: OrderBookSideSnapshot): String =
    if (snapshot.isEmpty) "empty"
    else snapshot.toVector.sortBy(_._1).map { case (price, os) => s"$price: ${os.mkString(", ")}" }.mkString("  ", "\n  ", "")

  private def withLevelDb[T](dataDirectory: String)(f: LevelDb[Id] => T): T =
    Using.resource(openDb(dataDirectory)) { db =>
      f(LevelDb.sync(db))
    }

  private def sendRequest(url: String, key: String, method: String = "get"): String = {
    print(s"Sending ${method.toUpperCase} $url... Response: ")
    val r = basicRequest.headers(Map("X-API-KEY" -> key))

    val body = method match {
      case "post" => r.post(uri"$url").send(backend).body
      case "get" => r.get(uri"$url").send(backend).body
      case "delete" => r.delete(uri"$url").send(backend).body
    }

    body match {
      case Right(x) => println(x); x
      case Left(e) => println(s"ERROR: $e"); System.exit(1); e
    }
  }

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

}
