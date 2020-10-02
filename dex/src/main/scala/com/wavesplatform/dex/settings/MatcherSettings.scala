package com.wavesplatform.dex.settings

import java.io.File

import cats.data.NonEmptyList
import cats.implicits.{catsSyntaxOptionId, none}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.typesafe.config.Config
import com.wavesplatform.dex.actors.address.AddressActor
import com.wavesplatform.dex.api.http.OrderBookHttpInfo
import com.wavesplatform.dex.db.AccountStorage.Settings.{valueReader => accountStorageSettingsReader}
import com.wavesplatform.dex.db.{AccountStorage, OrderDB}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair._
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import com.wavesplatform.dex.model.OrderValidator.exchangeTransactionCreationFee
import com.wavesplatform.dex.settings
import com.wavesplatform.dex.settings.DenormalizedMatchingRule.denormalizedMatchingRuleNelReader
import com.wavesplatform.dex.settings.DeviationsSettings._
import com.wavesplatform.dex.settings.EventsQueueSettings.eventsQueueSettingsReader
import com.wavesplatform.dex.settings.OrderFeeSettings._
import com.wavesplatform.dex.settings.OrderHistorySettings._
import com.wavesplatform.dex.settings.OrderRestrictionsSettings.orderRestrictionsSettingsReader
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.ConfigReaderMyOps
import com.wavesplatform.dex.settings.utils.{ConfigCursorsOps, ConfigReaders, ConfigSettingsValidator, RawFailureReason, validationOf}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.ValueReader.generatedReader
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}
import pureconfig.ConfigReader.Result
import pureconfig.configurable.genericMapReader
import pureconfig.error.{ExceptionThrown, FailureReason}
import pureconfig.generic.auto._
import pureconfig.generic.semiauto
import pureconfig.{ConfigCursor, ConfigObjectCursor, ConfigReader, Derivation}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex
import pureconfig.module.cats.nonEmptyListReader

case class MatcherSettings(
    id: String,
    addressSchemeCharacter: Char,
    accountStorage: AccountStorage.Settings,
    wavesBlockchainClient: WavesBlockchainClientSettings,
    ntpServer: String,
    restApi: RestAPISettings,
    exchangeTxBaseFee: Long,
    actorResponseTimeout: FiniteDuration,
    dataDirectory: String,
    snapshotsInterval: Int,
    limitEventsDuringRecovery: Option[Int],
    snapshotsLoadingTimeout: FiniteDuration,
    startEventsProcessingTimeout: FiniteDuration,
    orderBooksRecoveringTimeout: FiniteDuration,
    priceAssets: Seq[Asset],
    blacklistedAssets: Set[Asset.IssuedAsset],
    blacklistedNames: Seq[Regex],
    orderDb: OrderDB.Settings,
    // this is not a Set[Address] because to parse an address, global AddressScheme must be initialized
    blacklistedAddresses: Set[String],
    orderBookHttp: OrderBookHttpInfo.Settings,
    eventsQueue: EventsQueueSettings,
    processConsumedTimeout: FiniteDuration,
    orderFee: Map[Long, OrderFeeSettings],
    maxPriceDeviations: DeviationsSettings,
    orderRestrictions: Map[AssetPair, OrderRestrictionsSettings],
    matchingRules: Map[AssetPair, NonEmptyList[DenormalizedMatchingRule]],
    whiteListOnly: Boolean,
    allowedAssetPairs: Set[AssetPair],
    allowedOrderVersions: Set[Byte],
    exchangeTransactionBroadcast: ExchangeTransactionBroadcastSettings,
    postgres: PostgresConnection,
    orderHistory: Option[OrderHistorySettings],
    webSockets: WebSocketSettings,
    addressActor: AddressActor.Settings
) {

  val recoverOrderHistory = !new File(dataDirectory).exists()

  def mentionedAssets: Set[Asset] = {
    priceAssets.toSet ++
      blacklistedAssets ++
      orderRestrictions.keySet.flatMap(_.assets) ++
      matchingRules.keySet.flatMap(_.assets) ++
      allowedAssetPairs.flatMap(_.assets) ++
      orderFee.values.toSet[OrderFeeSettings].flatMap {
        case x: OrderFeeSettings.FixedSettings => Set(x.asset)
        case _                                 => Set.empty[Asset]
      }
  }
}

object MatcherSettings extends ConfigCursorsOps with ConfigReaders {

  implicit val byteStrConfigReader = byteStr58ConfigReader

  implicit val longOrderFeeConfigReader = genericMapReader[Long, OrderFeeSettings] { x =>
    x.toLongOption.fold[Either[FailureReason, Long]](RawFailureReason(s"'$x' should be numeric").asLeft)(_.asRight)
  }

  implicit val assetPairOrderRestrictionsConfigReader = genericMapReader[AssetPair, OrderRestrictionsSettings](assetPairKeyParser)

  implicit val matchingRulesConfigReader = genericMapReader[AssetPair, NonEmptyList[DenormalizedMatchingRule]](
    assetPairKeyParser
  )(Derivation.Successful(nonEmptyListReader[DenormalizedMatchingRule])) // To solve IntelliJ IDEA issue

  val exchangeTxBaseFeeValidation = validationOf[MatcherSettings, "exchangeTxBaseFee"].mk { settings =>
    if (settings.exchangeTxBaseFee >= exchangeTransactionCreationFee) none
    else s"base fee must be >= $exchangeTransactionCreationFee".some
  }

  val limitEventsDuringRecoveryValidation = validationOf[MatcherSettings, "limitEventsDuringRecovery"].mk { settings =>
    settings.limitEventsDuringRecovery match {
      case Some(value) if value < settings.snapshotsInterval =>
        s"$value should be >= snapshotsInterval: ${settings.snapshotsInterval}".some
      case _ => none
    }
  }

  implicit val matcherSettingsConfigReader: ConfigReader[MatcherSettings] = semiauto
    .deriveReader[MatcherSettings]
    .validated(
      exchangeTxBaseFeeValidation,
      limitEventsDuringRecoveryValidation
    )

  def assetPairKeyParser(x: String): Either[FailureReason, AssetPair] = AssetPair.extractAssetPair(x).toEither.leftMap(ExceptionThrown)

  // ======

  implicit val chosenCase: NameMapper                    = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit val valueReader: ValueReader[MatcherSettings] = (cfg, path) => fromConfig(cfg getConfig path)

  implicit val subscriptionsSettingsReader: ValueReader[SubscriptionsSettings] =
    com.wavesplatform.dex.settings.SubscriptionsSettings.subscriptionSettingsReader

  private def unsafeParseAsset(x: String): Asset = {
    AssetPair.extractAsset(x).getOrElse(throw new IllegalArgumentException(s"Can't parse '$x' as asset"))
  }

  private[this] def fromConfig(config: Config): MatcherSettings = {

    import ConfigSettingsValidator.AdhocValidation.{validateAssetPairKey, validateOffset}

    val addressSchemeCharacter =
      config
        .as[String]("address-scheme-character")
        .headOption
        .getOrElse(throw new IllegalArgumentException("waves.dex.address-scheme-character is mandatory!"))

    val accountStorage        = accountStorageSettingsReader.read(config, "account-storage")
    val wavesBlockchainClient = config.as[WavesBlockchainClientSettings]("waves-blockchain-client")
    val ntpServer             = config.as[String]("ntp-server")
    val restApiSettings       = config.as[RestAPISettings]("rest-api")

    val exchangeTxBaseFee = config.getValidatedByPredicate[Long]("exchange-tx-base-fee")(
      predicate = _ >= exchangeTransactionCreationFee,
      errorMsg = s"base fee must be >= $exchangeTransactionCreationFee"
    )

    val actorResponseTimeout = config.as[FiniteDuration]("actor-response-timeout")
    val dataDirectory        = config.as[String]("data-directory")
    val snapshotsInterval    = config.as[Int]("snapshots-interval")

    val limitEventsDuringRecovery = config.getAs[Int]("limit-events-during-recovery")
    require(limitEventsDuringRecovery.forall(_ >= snapshotsInterval), "limit-events-during-recovery should be >= snapshotsInterval")

    val snapshotsLoadingTimeout      = config.as[FiniteDuration]("snapshots-loading-timeout")
    val startEventsProcessingTimeout = config.as[FiniteDuration]("start-events-processing-timeout")
    val orderBooksRecoveringTimeout  = config.as[FiniteDuration]("order-books-recovering-timeout")
    val priceAssets                  = config.as[List[String]]("price-assets").map(unsafeParseAsset)

    val blacklistedAssets =
      config
        .as[List[String]]("blacklisted-assets")
        .map(unsafeParseAsset)
        .map {
          case Asset.Waves          => throw new IllegalArgumentException("Can't blacklist the main coin")
          case x: Asset.IssuedAsset => x
        }
        .toSet

    val blacklistedNames        = config.as[List[String]]("blacklisted-names").map(_.r)
    val blacklistedAddresses    = config.as[Set[String]]("blacklisted-addresses")
    val orderBookHttp           = config.as[OrderBookHttpInfo.Settings]("order-book-http")
    val eventsQueue             = config.as[EventsQueueSettings]("events-queue")
    val processConsumedTimeout  = config.as[FiniteDuration]("process-consumed-timeout")
    val orderFee                = config.getValidatedMap[Long, OrderFeeSettings]("order-fee")(validateOffset)
    val deviation               = config.as[DeviationsSettings]("max-price-deviations")
    val orderRestrictions       = config.getValidatedMap[AssetPair, OrderRestrictionsSettings]("order-restrictions")(validateAssetPairKey)
    val matchingRules           = config.getValidatedMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]("matching-rules")(validateAssetPairKey)
    val whiteListOnly           = config.as[Boolean]("white-list-only")
    val allowedAssetPairs       = config.getValidatedSet[AssetPair]("allowed-asset-pairs")
    val allowedOrderVersions    = config.as[Set[Int]]("allowed-order-versions").map(_.toByte)
    val broadcastUntilConfirmed = config.as[ExchangeTransactionBroadcastSettings]("exchange-transaction-broadcast")
    val postgresConnection      = config.as[PostgresConnection]("postgres")
    val orderHistory            = config.as[Option[OrderHistorySettings]]("order-history")
    val orderDb                 = config.as[OrderDB.Settings]("order-db")
    val webSocketSettings       = config.as[WebSocketSettings]("web-sockets")
    val addressActorSettings    = config.as[AddressActor.Settings]("address-actor")

    MatcherSettings(
      config.as[String]("id"),
      addressSchemeCharacter,
      accountStorage,
      wavesBlockchainClient,
      ntpServer,
      restApiSettings,
      exchangeTxBaseFee,
      actorResponseTimeout,
      dataDirectory,
      snapshotsInterval,
      limitEventsDuringRecovery,
      snapshotsLoadingTimeout,
      startEventsProcessingTimeout,
      orderBooksRecoveringTimeout,
      priceAssets,
      blacklistedAssets,
      blacklistedNames,
      orderDb,
      blacklistedAddresses,
      orderBookHttp,
      eventsQueue,
      processConsumedTimeout,
      orderFee,
      deviation,
      orderRestrictions,
      matchingRules,
      whiteListOnly,
      allowedAssetPairs,
      allowedOrderVersions,
      broadcastUntilConfirmed,
      postgresConnection,
      orderHistory,
      webSocketSettings,
      addressActorSettings
    )
  }

//  private type Offsets2FeeSettings = Map[Long, OrderFeeSettings]
//
//  private[this] def fromConfig1(cursor: ConfigCursor): MatcherSettings = {
//
//    import ConfigSettingsValidator.AdhocValidation.{validateAssetPairKey, validateOffset}
//
//    import pureconfig._
//    import pureconfig.configurable._
//    import pureconfig.ConvertHelpers._
//    import pureconfig.generic.auto._
//
//    def parseAssetsList(oc: ConfigObjectCursor)(path: String): Result[List[Asset]] =
//      for {
//        assetsStr <- oc.as[List[String]](path)
//        assets    <- assetsStr.traverse(as => AssetPair.extractAsset(as).toEither.leftFlatMap(_ => oc.failed(s"Can't parse '$as' as asset")))
//      } yield assets
//
//    val oc = cursor.asObjectCursor
//    val id = oc.as[String]("id")
//
//    for {
//      oc                           <- cursor.asObjectCursor
//      id                           <- oc.as[String]("id")
//      addressSchemeCharacter       <- oc.as[String]("address-scheme-character").map(_.head)
//      accountStorage               <- oc.as[AccountStorage.Settings]("account-storage")
//      wavesBlockchainClient        <- oc.as[WavesBlockchainClientSettings]("waves-blockchain-client")
//      ntpServer                    <- oc.as[String]("ntp-server")
//      restApiSettings              <- oc.as[RestAPISettings]("rest-api")
//      exchangeTxBaseFee            <- oc.as[Long]("exchange-tx-base-fee")
//      actorResponseTimeout         <- oc.as[FiniteDuration]("actor-response-timeout")
//      dataDirectory                <- oc.as[String]("data-directory")
//      snapshotsInterval            <- oc.as[Int]("snapshots-interval")
//      limitEventsDuringRecovery    <- oc.as[Option[Int]]("limit-events-during-recovery")
//      snapshotsLoadingTimeout      <- oc.as[FiniteDuration]("snapshots-loading-timeout")
//      startEventsProcessingTimeout <- oc.as[FiniteDuration]("start-events-processing-timeout")
//      orderBooksRecoveringTimeout  <- oc.as[FiniteDuration]("order-books-recovering-timeout")
//      priceAssets                  <- parseAssetsList(oc)("price-assets")
//      blacklistedAssets            <- parseAssetsList(oc)("blacklisted-assets").map(_.toSet[IssuedAsset])
//      blacklistedNames             <- oc.as[List[String]]("blacklisted-names").map(_.map(_.r))
//      orderDb                      <- oc.as[OrderDB.Settings]("order-db")
//      blacklistedAddresses         <- oc.as[Set[String]]("blacklisted-addresses")
//      orderBookHttp                <- oc.as[OrderBookHttpInfo.Settings]("order-book-http")
//      eventsQueue                  <- oc.as[EventsQueueSettings]("events-queue")
//      processConsumedTimeout       <- oc.as[FiniteDuration]("process-consumed-timeout")
//
//      implicit0(feeSettingsReader: ConfigReader[Offsets2FeeSettings]) = genericMapReader[Long, OrderFeeSettings](catchReadError(_.toLong))
//
//      orderFee <- oc.as[Offsets2FeeSettings]("order-fee")
//
//      recoverOrderHistory = !new File(dataDirectory).exists()
//
//      //      exchangeTxBaseFee <- oc
//      //        .as[Long]("exchange-tx-base-fee")
//      //        .ensure(oc.failed(s"base fee must be >= $exchangeTransactionCreationFee"))(_ >= exchangeTransactionCreationFee)
//
////      limitEventsDuringRecovery <- oc
////        .as[Option[Int]]("limit-events-during-recovery")
////        .ensure(oc.failed("limit-events-during-recovery should be >= snapshotsInterval"))(_.forall(_ >= snapshotsInterval))
//
////      blacklistedAssets <- parseAssetsList(oc)("blacklisted-assets")
////        .ensure(oc.failed("Can't blacklist the main coin"))(!_.contains(Waves))
////        .map(_.toSet[IssuedAsset])
//
//    } yield
//      MatcherSettings(
//        id,
//        addressSchemeCharacter,
//        accountStorage,
//        wavesBlockchainClient,
//        ntpServer,
//        restApiSettings,
//        exchangeTxBaseFee,
//        actorResponseTimeout,
//        dataDirectory,
//        recoverOrderHistory,
//        snapshotsInterval,
//        limitEventsDuringRecovery,
//        snapshotsLoadingTimeout,
//        startEventsProcessingTimeout,
//        orderBooksRecoveringTimeout,
//        priceAssets,
//        blacklistedAssets,
//        blacklistedNames,
//        orderDb,
//        blacklistedAddresses,
//        orderBookHttp,
//        eventsQueue,
//        processConsumedTimeout,
//        orderFee,
//        deviation,
//        orderRestrictions,
//        matchingRules,
//        whiteListOnly,
//        allowedAssetPairs,
//        allowedOrderVersions,
//        broadcastUntilConfirmed,
//        postgresConnection,
//        orderHistory,
//        webSocketSettings,
//        addressActorSettings
//      )
//
////    val actorResponseTimeout = config.as[FiniteDuration]("actor-response-timeout")
////    val dataDirectory        = config.as[String]("data-directory")
////    val recoverOrderHistory  = !new File(dataDirectory).exists()
////    val snapshotsInterval    = config.as[Int]("snapshots-interval")
////
////    val limitEventsDuringRecovery = config.getAs[Int]("limit-events-during-recovery")
////    require(limitEventsDuringRecovery.forall(_ >= snapshotsInterval), "limit-events-during-recovery should be >= snapshotsInterval")
//
////    val snapshotsLoadingTimeout      = config.as[FiniteDuration]("snapshots-loading-timeout")
////    val startEventsProcessingTimeout = config.as[FiniteDuration]("start-events-processing-timeout")
////    val orderBooksRecoveringTimeout  = config.as[FiniteDuration]("order-books-recovering-timeout")
////    val priceAssets                  = config.as[List[String]]("price-assets").map(unsafeParseAsset)
////
////    val blacklistedAssets =
////      config
////        .as[List[String]]("blacklisted-assets")
////        .map(unsafeParseAsset)
////        .map {
////          case Asset.Waves          => throw new IllegalArgumentException("Can't blacklist the main coin")
////          case x: Asset.IssuedAsset => x
////        }
////        .toSet
////
////    val blacklistedNames        = config.as[List[String]]("blacklisted-names").map(_.r)
////    val blacklistedAddresses    = config.as[Set[String]]("blacklisted-addresses")
////    val orderBookHttp           = config.as[OrderBookHttpInfo.Settings]("order-book-http")
////    val eventsQueue             = config.as[EventsQueueSettings]("events-queue")
////    val processConsumedTimeout  = config.as[FiniteDuration]("process-consumed-timeout")
//    val orderFee                = config.getValidatedMap[Long, OrderFeeSettings]("order-fee")(validateOffset)
//    val deviation               = config.as[DeviationsSettings]("max-price-deviations")
//    val orderRestrictions       = config.getValidatedMap[AssetPair, OrderRestrictionsSettings]("order-restrictions")(validateAssetPairKey)
//    val matchingRules           = config.getValidatedMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]("matching-rules")(validateAssetPairKey)
//    val whiteListOnly           = config.as[Boolean]("white-list-only")
//    val allowedAssetPairs       = config.getValidatedSet[AssetPair]("allowed-asset-pairs")
//    val allowedOrderVersions    = config.as[Set[Int]]("allowed-order-versions").map(_.toByte)
//    val broadcastUntilConfirmed = config.as[ExchangeTransactionBroadcastSettings]("exchange-transaction-broadcast")
//    val postgresConnection      = config.as[PostgresConnection]("postgres")
//    val orderHistory            = config.as[Option[OrderHistorySettings]]("order-history")
//    val orderDb                 = config.as[OrderDB.Settings]("order-db")
//    val webSocketSettings       = config.as[WebSocketSettings]("web-sockets")
//    val addressActorSettings    = config.as[AddressActor.Settings]("address-actor")
//
//    MatcherSettings(
//      config.as[String]("id"),
//      addressSchemeCharacter,
//      accountStorage,
//      wavesBlockchainClient,
//      ntpServer,
//      restApiSettings,
//      exchangeTxBaseFee,
//      actorResponseTimeout,
//      dataDirectory,
//      recoverOrderHistory,
//      snapshotsInterval,
//      limitEventsDuringRecovery,
//      snapshotsLoadingTimeout,
//      startEventsProcessingTimeout,
//      orderBooksRecoveringTimeout,
//      priceAssets,
//      blacklistedAssets,
//      blacklistedNames,
//      orderDb,
//      blacklistedAddresses,
//      orderBookHttp,
//      eventsQueue,
//      processConsumedTimeout,
//      orderFee,
//      deviation,
//      orderRestrictions,
//      matchingRules,
//      whiteListOnly,
//      allowedAssetPairs,
//      allowedOrderVersions,
//      broadcastUntilConfirmed,
//      postgresConnection,
//      orderHistory,
//      webSocketSettings,
//      addressActorSettings
//    )
//  }
}
