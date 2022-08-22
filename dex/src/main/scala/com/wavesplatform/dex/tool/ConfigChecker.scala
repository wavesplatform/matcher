package com.wavesplatform.dex.tool

import cats.Eval
import cats.data.NonEmptyList
import cats.syntax.either._
import com.typesafe.config.ConfigFactory.parseFile
import com.typesafe.config._
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.model.AssetPairQuickValidator
import com.wavesplatform.dex.settings.MatcherSettings.PartialOrderFeeSettings
import com.wavesplatform.dex.settings.OrderFeeSettings._
import com.wavesplatform.dex.settings._
import pureconfig.ConfigWriter
import pureconfig.configurable.genericMapWriter
import pureconfig.generic.auto._
import pureconfig.generic.semiauto
import pureconfig.module.cats.nonEmptyListWriter
import sttp.model.Uri

import java.io.File
import scala.jdk.CollectionConverters._

object ConfigChecker extends ConfigWriters {

  def checkConfig(configPath: String): ErrorOr[Seq[String]] = {
    val file = new File(configPath)
    if (file.exists())
      checkConfig(parseFile(file))
    else s"File with configs $configPath not found".asLeft
  }

  def checkConfig(config: Config): ErrorOr[Seq[String]] =
    loadMatcherSettings(config).flatMap(ms => checkConfig(config, ms))

  def checkConfig(config: Config, matcherSettings: MatcherSettings): ErrorOr[Seq[String]] =
    Either.catchNonFatal {
      val skippedPaths = matcherSettings.cli.ignoreUnusedProperties
      val usingProperties: ConfigValue = ConfigWriter[MatcherSettings].to(matcherSettings)
      val allProperties = config.getConfig("waves.dex").entrySet()
      getConfigObject(usingProperties)
        .map { co =>
          allProperties.asScala.toSeq
            .filter(e => skippedPaths.forall(b => !e.getKey.startsWith(b)))
            .foldLeft(List.empty[String]) { (acc, elem) =>
              val key = elem.getKey
              if (checkProperty(key, co).value)
                acc
              else
                key :: acc
            }
        }
        .getOrElse(List.empty)
    }.leftMap(ex => s"Cannot check settings: ${ex.getWithStackTrace}")

  private def getConfigObject(cv: ConfigValue): Option[ConfigObject] =
    cv match {
      case co: ConfigObject => Some(co)
      case _ => None
    }

  private def checkProperty(property: String, cfg: ConfigObject): Eval[Boolean] = {
    val firstDotIndex = property.indexOf('.')
    if (firstDotIndex > 0) {
      val (root, branch) = property.splitAt(firstDotIndex)
      val pureBranch = branch.substring(1)
      Eval.defer(
        Option(cfg.get(root))
          .flatMap(getConfigObject)
          .fold(Eval.now(false))(co => checkProperty(pureBranch, co))
      )
    } else Eval.now(Option(cfg.get(property)).nonEmpty)
  }

}

sealed trait ConfigWriters {

  val byteStr58ConfigWriter: ConfigWriter[ByteStr] = ConfigWriter.toString(_.base58)
  val byteStr64ConfigWriter: ConfigWriter[ByteStr] = ConfigWriter.toString(_.base64)

  implicit val assetConfigWriter: ConfigWriter[Asset] = ConfigWriter.toString(assetToString)

  implicit val issuedAssetConfigWriter: ConfigWriter[Asset.IssuedAsset] =
    byteStr58ConfigWriter.contramap(_.id)

  implicit val assetPairConfigWriter: ConfigWriter[AssetPair] =
    ConfigWriter.toString(assetPairToString)

  implicit val publicKeyWriter: ConfigWriter[PublicKey] =
    byteStr58ConfigWriter.contramap(_.asInstanceOf[ByteStr])

  implicit val uriWriter: ConfigWriter[Uri] =
    ConfigWriter.toString[Uri](u => u.toString())

  implicit val allOrderFeeSettingsWriter: ConfigWriter[AllOrderFeeSettings] =
    semiauto.deriveWriter[AllOrderFeeSettings]

  implicit val orderFeeWriter: ConfigWriter[OrderFeeSettings] =
    ConfigWriter.fromFunction {
      case a: DynamicSettings =>
        allOrderFeeSettingsWriter.to(
          AllOrderFeeSettings("dynamic", Some(a), None, None, None)
        )

      case a: PercentSettings =>
        allOrderFeeSettingsWriter.to(
          AllOrderFeeSettings("percent", None, Some(a), None, None)
        )

      case a: FixedSettings =>
        allOrderFeeSettingsWriter.to(
          AllOrderFeeSettings("fixed", None, None, Some(a), None)
        )

      case a: CompositeSettings =>
        allOrderFeeSettingsWriter.to(
          AllOrderFeeSettings("composite", None, None, None, Some(a))
        )
    }

  implicit val discountAssetSettingsWriter: ConfigWriter[CompositeSettings.DiscountAssetSettings] =
    semiauto.deriveWriter[CompositeSettings.DiscountAssetSettings]

  implicit val allAccStorageSettingsWriter: ConfigWriter[AllAccountStorageSettings] =
    semiauto.deriveWriter[AllAccountStorageSettings]

  implicit val accountStorageSettingsWriter: ConfigWriter[AccountStorage.Settings] =
    ConfigWriter.fromFunction {
      case a: AccountStorage.Settings.InMem =>
        allAccStorageSettingsWriter.to(
          AllAccountStorageSettings(
            "in-mem",
            a,
            AccountStorage.Settings.EncryptedFile.empty
          )
        )

      case a: AccountStorage.Settings.EncryptedFile =>
        allAccStorageSettingsWriter.to(
          AllAccountStorageSettings(
            "encrypted-file",
            AccountStorage.Settings.InMem.empty,
            a
          )
        )
    }

  implicit val orderFeeSettingsWriter: ConfigWriter[PartialOrderFeeSettings] =
    orderFeeWriter
      .contramap[PartialOrderFeeSettings](_(_ => true))

  implicit val longOrderFeeConfigWriter: ConfigWriter[Map[Long, PartialOrderFeeSettings]] = genericMapWriter(_.toString)

  implicit val assetPairOrderFeeSettingsConfigWriter: ConfigWriter[Map[AssetPair, OrderFeeSettings]] =
    genericMapWriter(assetPairToString)

  implicit val assetPairOrderRestrictionsConfigWriter: ConfigWriter[Map[AssetPair, OrderRestrictionsSettings]] =
    genericMapWriter(assetPairToString)

  implicit val denormalizedMatchingRuleConfigWriter: ConfigWriter[NonEmptyList[DenormalizedMatchingRule]] =
    nonEmptyListWriter[DenormalizedMatchingRule]

  implicit val matchingRulesConfigWriter: ConfigWriter[Map[AssetPair, NonEmptyList[DenormalizedMatchingRule]]] =
    genericMapWriter[AssetPair, NonEmptyList[DenormalizedMatchingRule]](
      assetPairToString
    )

  implicit val matcherSettingsConfigWriter: ConfigWriter[MatcherSettings] =
    semiauto.deriveWriter[MatcherSettings]

  implicit val compositeSettingsConfigWriter: ConfigWriter[CompositeSettings] = ConfigWriter.fromFunction { settings =>
    ConfigValueFactory.fromMap(
      Map(
        "default" -> orderFeeWriter.to(settings.default),
        "custom" -> genericMapWriter[AssetPair, OrderFeeSettings](assetPairToString).to(settings.custom),
        "zero-fee-accounts" -> implicitly[ConfigWriter[Set[PublicKey]]].to(settings.zeroFeeAccounts),
        "discount" -> implicitly[ConfigWriter[Option[CompositeSettings.DiscountAssetSettings]]].to(settings.discount)
      ).asJava
    )
  }

  private def assetToString(asset: Asset): String =
    asset match {
      case Asset.Waves => Asset.WavesName
      case Asset.IssuedAsset(id) => id.base58
    }

  protected def assetPairToString(assetPair: AssetPair): String = {
    val amountAssetStr = assetToString(assetPair.amountAsset)
    val priceAssetStr = assetToString(assetPair.priceAsset)
    s"$amountAssetStr-$priceAssetStr"
  }

}

case class AllOrderFeeSettings(
  mode: String,
  dynamic: Option[DynamicSettings],
  percent: Option[PercentSettings],
  fixed: Option[FixedSettings],
  composite: Option[CompositeSettings]
)

case class AllAccountStorageSettings(
  `type`: String,
  inMem: AccountStorage.Settings.InMem,
  encryptedFile: AccountStorage.Settings.EncryptedFile
)
