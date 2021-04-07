package com.wavesplatform.dex.tool

import cats.Eval
import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory.parseFile
import com.typesafe.config._
import com.wavesplatform.dex.settings._
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import cats.syntax.either._
import com.wavesplatform.dex.db.AccountStorage
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.settings.OrderFeeSettings._
import pureconfig.ConfigWriter
import pureconfig._
import pureconfig.generic.auto._
import pureconfig.configurable.genericMapWriter
import pureconfig.generic.semiauto
import pureconfig.module.cats.nonEmptyListWriter
import sttp.model.Uri

import scala.jdk.CollectionConverters._
import java.io.File
import scala.util.Try

object ConfigChecker extends ConfigWriters {

  val skippedPaths = Seq(
    "events-queue.kafka.consumer.client",
    "events-queue.kafka.producer.client"
  )

  def checkConfig(configPath: String): ErrorOr[Seq[String]] =
    checkConfig(parseFile(new File(configPath)))

  def checkConfig(rawCfg: Config): ErrorOr[Seq[String]] =
    loadMatcherSettings(rawCfg).toEither.leftMap(ex => s"Cannot load matcher settings: ${ex.getWithStackTrace}")
      .flatMap(ms => checkConfig(rawCfg, ms))

  def checkConfig(rawCfg: Config, matcherSettings: MatcherSettings): ErrorOr[Seq[String]] =
    Try {
      val usingProperties: ConfigValue = ConfigWriter[MatcherSettings].to(matcherSettings)
      val allProperties = rawCfg.getConfig("waves.dex").entrySet()
      getConfigObject(usingProperties)
        .map { co =>
          allProperties.asScala.toSeq
            .filter(e => skippedPaths.forall(b => !e.getKey.startsWith(b)))
            .foldLeft(List.empty[String]) { (acc, elem) =>
              val key = elem.getKey
              if (checkProperty(key, co).value) {
                acc
              } else {
                acc :+ key
              }
            }
        }
        .getOrElse(List.empty)
    }.toEither.leftMap(ex => s"Cannot check settings: ${ex.getWithStackTrace}")

  private def getConfigObject(cv: ConfigValue): Option[ConfigObject] =
    cv match {
      case co: ConfigObject => Some(co)
      case _                => None
    }

  private def checkProperty(property: String, cfg: ConfigObject): Eval[Boolean] = {
    val firstDotIndex = property.indexOf('.')
    if (firstDotIndex > 0) {
      val (root, branch) = property.splitAt(firstDotIndex)
      val pureBranch = branch.substring(1)
      Eval.defer(
        Option(cfg.get(root))
          .flatMap(getConfigObject)
          .fold(Eval.now(false))(co => checkProperty(pureBranch, co)))
    } else Eval.now(Option(cfg.get(property)).nonEmpty)
  }

}

trait ConfigWriters {

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

  implicit val orderFeeSettingsAdtWriter: ConfigWriter[OrderFeeSettingsAdt] =
    semiauto.deriveWriter[OrderFeeSettingsAdt]

  implicit val orderFeeWriter: ConfigWriter[OrderFeeSettings] =
    ConfigWriter.fromFunction {
      case a: DynamicSettings =>
        orderFeeSettingsAdtWriter.to(
          OrderFeeSettingsAdt(
            "dynamic",
            a,
            PercentSettings.empty,
            FixedSettings.empty
          )
        )

      case a: PercentSettings =>
        orderFeeSettingsAdtWriter.to(
          OrderFeeSettingsAdt("percent", DynamicSettings.empty, a, FixedSettings.empty))

      case a: FixedSettings =>
        orderFeeSettingsAdtWriter.to(
          OrderFeeSettingsAdt("fixed", DynamicSettings.empty, PercentSettings.empty, a))

    }

  implicit val accStorageSettingsAdtWriter: ConfigWriter[AccountStorageSettingsAdt] =
    semiauto.deriveWriter[AccountStorageSettingsAdt]

  implicit val accountStorageSettingsWriter: ConfigWriter[AccountStorage.Settings] =
    ConfigWriter.fromFunction {
      case a: AccountStorage.Settings.InMem =>
        accStorageSettingsAdtWriter.to(
          AccountStorageSettingsAdt(
            "in-mem",
            a,
            AccountStorage.Settings.EncryptedFile.empty
          )
        )

      case a: AccountStorage.Settings.EncryptedFile =>
        accStorageSettingsAdtWriter.to(
          AccountStorageSettingsAdt(
            "encrypted-file",
            AccountStorage.Settings.InMem.empty,
            a
          )
        )
    }


  implicit val longOrderFeeConfigWriter: ConfigWriter[Map[Long, OrderFeeSettings]] = genericMapWriter(_.toString)
  implicit val assetPairOrderRestrictionsConfigWriter: ConfigWriter[Map[AssetPair, OrderRestrictionsSettings]] =
    genericMapWriter(assetPairToString)

  implicit val denormalizedMatchingRuleConfigWriter: ConfigWriter[NonEmptyList[DenormalizedMatchingRule]] =
    nonEmptyListWriter[DenormalizedMatchingRule]

  implicit val matchingRulesConfigWriter: ConfigWriter[Map[AssetPair, NonEmptyList[DenormalizedMatchingRule]]] =
    genericMapWriter[AssetPair, NonEmptyList[DenormalizedMatchingRule]](
      assetPairToString)

  implicit val matcherSettingsConfigWriter: ConfigWriter[MatcherSettings] =
    semiauto.deriveWriter[MatcherSettings]

  private def assetToString(asset: Asset): String = {
    asset match {
      case Asset.Waves           => Asset.WavesName
      case Asset.IssuedAsset(id) => id.base58
    }
  }

  protected def assetPairToString(assetPair: AssetPair): String = {
    val amountAssetStr = assetToString(assetPair.amountAsset)
    val priceAssetStr = assetToString(assetPair.priceAsset)
    s"$amountAssetStr-$priceAssetStr"
  }

}

case class OrderFeeSettingsAdt(
    mode: String,
    dynamic: DynamicSettings,
    percent: PercentSettings,
    fixed: FixedSettings
  )

case class AccountStorageSettingsAdt(
    `type`: String,
    inMem: AccountStorage.Settings.InMem,
    encryptedFile: AccountStorage.Settings.EncryptedFile
  )
