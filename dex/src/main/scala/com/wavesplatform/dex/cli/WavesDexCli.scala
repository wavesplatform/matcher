package com.wavesplatform.dex.cli

import cats.instances.either._
import cats.syntax.either._
import cats.syntax.option._
import com.typesafe.config.ConfigFactory.parseFile
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex._
import com.wavesplatform.dex.cli.Actions._
import com.wavesplatform.dex.domain.account.AddressScheme
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.settings.{loadMatcherSettings, MatcherSettings}
import scopt.{OParser, OParserBuilder, RenderingMode}

import java.io.File
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

object WavesDexCli extends ScoptImplicits {

  // todo commands:
  // get account by seed [and nonce]
  def main(rawArgs: Array[String]): Unit = {
    val builder: OParserBuilder[Args] = OParser.builder[Args]

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
        cmdGenerateAccountSeed(builder),
        cmdCreateAccountStorage(builder),
        cmdCreateDocumentation(builder),
        cmdCreateApiKey(builder),
        cmdCheckServer(builder),
        cmdRunComparison(builder),
        cmdMakeOrderbookSnapshots(builder),
        cmdCheckConfigFile(builder),
        cmdCleanAssets(builder),
        cmdInspectAsset(builder),
        cmdSetAsset(builder),
        cmdListAssetPairs(builder),
        cmdInspectOrderBook(builder),
        cmdDeleteOrderBookFromLevelDB(builder),
        cmdLowestSnapshotsOffset(builder),
        cmdInspectOrder(builder),
        cmdGenerateFeeSettings(builder),
        cmdDeleteOrderbook(builder)
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
              case Command.DeleteOrderBookFromLevelDB => deleteOrderBookFromLevelDb(args, matcherSettings)
              case Command.LowestSnapshotsOffset => lowestSnapshotsOffset(args, matcherSettings)
              case Command.InspectOrder => inspectOrder(args, matcherSettings)
              case Command.GenerateFeeSettings => generateFeeSettings(args)
              case Command.DeleteOrderBook => deleteOrderBookFromLevelDb(args, matcherSettings)
            }
            println("Done")
        }
      }
  }

  private def cmdDeleteOrderbook(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.DeleteOrderBook.name)
      .action((_, s) => s.copy(command = Command.DeleteOrderBook.some))
      .text("Compares the data from multiple matchers")
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
        opt[String]("asset-pair")
          .abbr("ap")
          .text("An asset pair of order book")
          .valueName("<amount-asset-id-in-base58>-<price-asset-id-in-base58>")
          .required()
          .action((x, s) => s.copy(assetPair = x)),
        opt[FiniteDuration]("timeout")
          .abbr("to")
          .text("Timeout")
          .valueName("<raw-string>")
          .action((x, s) => s.copy(timeout = x))
      )
  }

  private def cmdRunComparison(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdMakeOrderbookSnapshots(builder: OParserBuilder[Args]) = {
    import builder._
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
  }

  private def cmdCheckConfigFile(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdCleanAssets(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdInspectAsset(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdSetAsset(builder: OParserBuilder[Args]) = {
    import builder._
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
          .action((_, s) => s.copy(isNft = true))
      )
  }

  private def cmdListAssetPairs(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdInspectOrderBook(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdDeleteOrderBookFromLevelDB(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.DeleteOrderBookFromLevelDB.name)
      .action((_, s) => s.copy(command = Command.DeleteOrderBookFromLevelDB.some))
      .text("Deletes an order book from levelDB")
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
      )
  }

  private def cmdLowestSnapshotsOffset(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.LowestSnapshotsOffset.name)
      .action((_, s) => s.copy(command = Command.LowestSnapshotsOffset.some))
      .text("Finds lowest snapshots offset")
      .children(
        opt[String]("dex-config")
          .abbr("dc")
          .text("DEX config path")
          .valueName("<raw-string>")
          .required()
          .action((x, s) => s.copy(configPath = x))
      )
  }

  private def cmdInspectOrder(builder: OParserBuilder[Args]) = {
    import builder._
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
  }

  private def cmdGenerateFeeSettings(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.GenerateFeeSettings.name)
      .action((_, s) => s.copy(command = Command.GenerateFeeSettings.some))
      .text("Generate fee settings")
      .children(
        opt[Seq[String]]("amount-assets")
          .valueName("<list of base58-encoded asset ids>")
          .required()
          .action((x, s) => s.copy(amountAssets = x)),
        opt[Seq[String]]("price-assets")
          .valueName("<list of base58-encoded asset ids>")
          .required()
          .action((x, s) => s.copy(priceAssets = x)),
        opt[Double]("min-fee")
          .valueName("<double value>")
          .required()
          .action((x, s) => s.copy(minFee = x)),
        opt[Long]("min-fee-in-waves")
          .valueName("<long value>")
          .required()
          .action((x, s) => s.copy(minFeeInWaves = x))
      )
  }

  private def cmdCheckServer(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdCreateApiKey(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.CreateApiKey.name)
      .action((_, s) => s.copy(command = Command.CreateApiKey.some))
      .text("Creates a hashed version of api key and prints settings for DEX server to change it")
      .children(
        opt[String]("api-key")
          .abbr("ak")
          .text("Raw API key, which will be passed to REST API in the X-Api-Key header")
          .required()
          .action((x, s) => s.copy(apiKey = x))
      )
  }

  private def cmdCreateDocumentation(builder: OParserBuilder[Args]) = {
    import builder._
    cmd(Command.CreateDocumentation.name)
      .action((_, s) => s.copy(command = Command.CreateDocumentation.some))
      .text("Creates a documentation about errors and writes it to the output directory")
      .children(
        opt[File]("output-directory")
          .abbr("od")
          .text("Where to save the documentation")
          .required()
          .action((x, s) => s.copy(outputDirectory = x))
      )
  }

  private def cmdCreateAccountStorage(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
  }

  private def cmdGenerateAccountSeed(builder: OParserBuilder[Args]) = {
    import builder._
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
      )
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

    case object DeleteOrderBookFromLevelDB extends Command {
      override def name: String = "delete-orderbook-from-level-db"
    }

    case object DeleteOrderBook extends Command {
      override def name: String = "delete-orderbook"
    }

    case object LowestSnapshotsOffset extends Command {
      override def name: String = "lowest-snapshots-offset"
    }

    case object InspectOrder extends Command {
      override def name: String = "inspect-order"
    }

    case object GenerateFeeSettings extends Command {
      override def name: String = "generate-fee-settings"
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
    timeout: FiniteDuration = 0 seconds,
    amountAssets: Seq[String] = Seq.empty,
    priceAssets: Seq[String] = Seq.empty,
    minFee: Double = 0.01,
    minFeeInWaves: Long = 1000000
  )

}
