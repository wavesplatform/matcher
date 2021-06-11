package com.wavesplatform.dex.settings

import cats.data.NonEmptyList
import cats.implicits._
import com.wavesplatform.dex.actors.address.AddressActor
import com.wavesplatform.dex.actors.events.OrderEventsCoordinatorActor
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor
import com.wavesplatform.dex.api.http.OrderBookHttpInfo
import com.wavesplatform.dex.db.{AccountStorage, OrderDb}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import com.wavesplatform.dex.model.OrderValidator.exchangeTransactionCreationFee
import com.wavesplatform.dex.settings.utils.ConfigReaderOps.Implicits
import com.wavesplatform.dex.settings.utils.{validationOf, ConfigReaders, RawFailureReason}
import com.wavesplatform.dex.tool.ComparisonTool
import pureconfig.ConfigReader
import pureconfig.configurable.genericMapReader
import pureconfig.error.{ExceptionThrown, FailureReason}
import pureconfig.generic.auto._
import pureconfig.generic.semiauto
import pureconfig.module.cats.nonEmptyListReader

import java.io.File
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

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
  waitingOffsetTool: WaitingOffsetToolSettings,
  priceAssets: Seq[Asset],
  blacklistedAssets: Set[Asset.IssuedAsset],
  blacklistedNames: Seq[Regex],
  orderDb: OrderDb.Settings,
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
  exchangeTransactionBroadcast: ExchangeTransactionBroadcastActor.Settings,
  postgres: PostgresConnection,
  orderHistory: OrderHistorySettings,
  webSockets: WebSocketSettings,
  addressActor: AddressActor.Settings,
  orderEventsCoordinatorActor: OrderEventsCoordinatorActor.Settings,
  comparisonTool: ComparisonTool.Settings,
  cli: CliSettings,
  secureKeys: Set[String]
) {

  val recoverOrderHistory = !new File(dataDirectory).exists()

  def mentionedAssets: Set[Asset] =
    priceAssets.toSet ++
    blacklistedAssets ++
    orderRestrictions.keySet.flatMap(_.assets) ++
    matchingRules.keySet.flatMap(_.assets) ++
    allowedAssetPairs.flatMap(_.assets) ++
    orderFee.values.toSet[OrderFeeSettings].flatMap {
      case x: OrderFeeSettings.FixedSettings => Set(x.asset)
      case _ => Set.empty[Asset]
    }

}

object MatcherSettings extends ConfigReaders {

  implicit val byteStrConfigReader = byteStr58ConfigReader

  implicit val longOrderFeeConfigReader = genericMapReader[Long, OrderFeeSettings] { x =>
    x.toLongOption.fold[Either[FailureReason, Long]](RawFailureReason(s"'$x' should be numeric").asLeft)(_.asRight)
  }

  implicit val assetPairOrderRestrictionsConfigReader = genericMapReader[AssetPair, OrderRestrictionsSettings](assetPairKeyParser)

  // TODO Create a case class for this
  implicit val denormalizedMatchingRuleConfigReader = nonEmptyListReader[DenormalizedMatchingRule].validatedList(
    validationOf.list[NonEmptyList[DenormalizedMatchingRule]].mk { xs =>
      val isStrictOrder = xs.tail.zip(xs.toList).forall { case (next, prev) => next.startOffset > prev.startOffset }
      if (isStrictOrder) none
      else s"Rules should be ordered by offset, but they are: ${xs.map(_.startOffset).toList.mkString(", ")}".some
    }
  )

  implicit val matchingRulesConfigReader = genericMapReader[AssetPair, NonEmptyList[DenormalizedMatchingRule]](assetPairKeyParser)

  val exchangeTxBaseFeeValidation = validationOf.field[MatcherSettings, "exchangeTxBaseFee"].mk { settings =>
    if (settings.exchangeTxBaseFee >= exchangeTransactionCreationFee) none
    else s"base fee must be >= $exchangeTransactionCreationFee".some
  }

  val limitEventsDuringRecoveryValidation = validationOf.field[MatcherSettings, "limitEventsDuringRecovery"].mk { settings =>
    settings.limitEventsDuringRecovery match {
      case Some(value) if value < settings.snapshotsInterval =>
        s"$value should be >= snapshotsInterval: ${settings.snapshotsInterval}".some
      case _ => none
    }
  }

  implicit val matcherSettingsConfigReader: ConfigReader[MatcherSettings] = semiauto
    .deriveReader[MatcherSettings]
    .validatedField(
      exchangeTxBaseFeeValidation,
      limitEventsDuringRecoveryValidation
    )

  def assetPairKeyParser(x: String): Either[FailureReason, AssetPair] = AssetPair.extractAssetPair(x).toEither.leftMap(ExceptionThrown)
}
