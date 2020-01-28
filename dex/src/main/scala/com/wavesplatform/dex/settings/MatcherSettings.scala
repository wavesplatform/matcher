package com.wavesplatform.dex.settings

import java.io.File

import cats.data.NonEmptyList
import com.typesafe.config.Config
import com.wavesplatform.dex.api.OrderBookSnapshotHttpCache
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.db.AccountStorage.Settings.{valueReader => accountStorageSettingsReader}
import com.wavesplatform.dex.domain.asset.AssetPair._
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.DenormalizedMatchingRule.denormalizedMatchingRuleNelReader
import com.wavesplatform.dex.settings.DeviationsSettings._
import com.wavesplatform.dex.settings.EventsQueueSettings.eventsQueueSettingsReader
import com.wavesplatform.dex.settings.OrderFeeSettings.{OrderFeeSettings, _}
import com.wavesplatform.dex.settings.OrderHistorySettings._
import com.wavesplatform.dex.settings.OrderRestrictionsSettings.orderRestrictionsSettingsReader
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.utils.ConfigSettingsValidator
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.{NameMapper, ValueReader}

import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

case class MatcherSettings(addressSchemeCharacter: Char,
                           accountStorage: AccountStorage.Settings,
                           wavesBlockchainClient: WavesBlockchainClientSettings,
                           ntpServer: String,
                           restApi: RestAPISettings,
                           exchangeTxBaseFee: Long,
                           actorResponseTimeout: FiniteDuration,
                           dataDir: String,
                           recoverOrderHistory: Boolean,
                           snapshotsInterval: Int,
                           limitEventsDuringRecovery: Option[Int],
                           snapshotsLoadingTimeout: FiniteDuration,
                           startEventsProcessingTimeout: FiniteDuration,
                           orderBooksRecoveringTimeout: FiniteDuration,
                           priceAssets: Seq[Asset],
                           blacklistedAssets: Set[Asset.IssuedAsset],
                           blacklistedNames: Seq[Regex],
                           maxOrdersPerRequest: Int,
                           // this is not a Set[Address] because to parse an address, global AddressScheme must be initialized
                           blacklistedAddresses: Set[String],
                           orderBookSnapshotHttpCache: OrderBookSnapshotHttpCache.Settings,
                           eventsQueue: EventsQueueSettings,
                           processConsumedTimeout: FiniteDuration,
                           orderFee: Map[Long, OrderFeeSettings],
                           deviation: DeviationsSettings,
                           orderRestrictions: Map[AssetPair, OrderRestrictionsSettings],
                           matchingRules: Map[AssetPair, NonEmptyList[DenormalizedMatchingRule]],
                           whiteListOnly: Boolean,
                           allowedAssetPairs: Set[AssetPair],
                           allowedOrderVersions: Set[Byte],
                           exchangeTransactionBroadcast: ExchangeTransactionBroadcastSettings,
                           postgresConnection: PostgresConnection,
                           orderHistory: Option[OrderHistorySettings]) {

  def mentionedAssets: Set[Asset] = {
    priceAssets.toSet ++
      blacklistedAssets ++
      orderRestrictions.keySet.flatMap(_.assets) ++
      matchingRules.keySet.flatMap(_.assets) ++
      allowedAssetPairs.flatMap(_.assets) ++
      orderFee.values.toSet[OrderFeeSettings].flatMap {
        case x: OrderFeeSettings.FixedSettings => Set(x.defaultAsset)
        case _                                 => Set.empty[Asset]
      }
  }
}

case class RestAPISettings(address: String, port: Int, apiKeyHash: String, cors: Boolean, apiKeyDifferentHost: Boolean)

object MatcherSettings {

  implicit val chosenCase: NameMapper                    = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit val valueReader: ValueReader[MatcherSettings] = (cfg, path) => fromConfig(cfg getConfig path)

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
      predicate = _ >= OrderValidator.exchangeTransactionCreationFee,
      errorMsg = s"base fee must be >= ${OrderValidator.exchangeTransactionCreationFee}"
    )

    val actorResponseTimeout = config.as[FiniteDuration]("actor-response-timeout")
    val dataDirectory        = config.as[String]("data-directory")
    val recoverOrderHistory  = !new File(dataDirectory).exists()
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

    val blacklistedNames           = config.as[List[String]]("blacklisted-names").map(_.r)
    val maxOrdersPerRequest        = config.as[Int]("rest-order-limit")
    val blacklistedAddresses       = config.as[Set[String]]("blacklisted-addresses")
    val orderBookSnapshotHttpCache = config.as[OrderBookSnapshotHttpCache.Settings]("order-book-snapshot-http-cache")
    val eventsQueue                = config.as[EventsQueueSettings]("events-queue")
    val processConsumedTimeout     = config.as[FiniteDuration]("process-consumed-timeout")
    val orderFee                   = config.getValidatedMap[Long, OrderFeeSettings]("order-fee")(validateOffset)
    val deviation                  = config.as[DeviationsSettings]("max-price-deviations")
    val orderRestrictions          = config.getValidatedMap[AssetPair, OrderRestrictionsSettings]("order-restrictions")(validateAssetPairKey)
    val matchingRules              = config.getValidatedMap[AssetPair, NonEmptyList[DenormalizedMatchingRule]]("matching-rules")(validateAssetPairKey)
    val whiteListOnly              = config.as[Boolean]("white-list-only")
    val allowedAssetPairs          = config.getValidatedSet[AssetPair]("allowed-asset-pairs")
    val allowedOrderVersions       = config.as[Set[Int]]("allowed-order-versions").map(_.toByte)
    val broadcastUntilConfirmed    = config.as[ExchangeTransactionBroadcastSettings]("exchange-transaction-broadcast")
    val postgresConnection         = config.as[PostgresConnection]("postgres")
    val orderHistory               = config.as[Option[OrderHistorySettings]]("order-history")

    MatcherSettings(
      addressSchemeCharacter,
      accountStorage,
      wavesBlockchainClient,
      ntpServer,
      restApiSettings,
      exchangeTxBaseFee,
      actorResponseTimeout,
      dataDirectory,
      recoverOrderHistory,
      snapshotsInterval,
      limitEventsDuringRecovery,
      snapshotsLoadingTimeout,
      startEventsProcessingTimeout,
      orderBooksRecoveringTimeout,
      priceAssets,
      blacklistedAssets,
      blacklistedNames,
      maxOrdersPerRequest,
      blacklistedAddresses,
      orderBookSnapshotHttpCache,
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
      orderHistory
    )
  }
}
