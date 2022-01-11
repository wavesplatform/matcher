package com.wavesplatform.dex.utils

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.fp.MayBeEmpty
import com.wavesplatform.dex.utils.FormatUtils.formatValue
import com.wavesplatform.transactions.{ExchangeTransaction, Transaction}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object JsonImplicits {

  implicit final class JsPathOps(val self: JsPath) extends AnyVal {

    def formatMayBeEmpty[T](implicit f: Format[T], mayBeEmpty: MayBeEmpty[T]): OFormat[T] =
      self
        .formatNullable[T]
        .inmap[T](
          _.fold(mayBeEmpty.empty)(identity),
          Option(_).filterNot(mayBeEmpty.isEmpty)
        )

  }

  implicit def eitherFormat[L, R](implicit lFormat: Format[L], rFormat: Format[R], ctl: ClassTag[L], ctr: ClassTag[R]): Format[Either[L, R]] =
    Format(
      Reads { js =>
        js.validate[R]
          .map(Right[L, R])
          .orElse {
            js.validate[L].map(Left[L, R])
          }
          .orElse(JsError(s"Can't parse as Either[${ctl.runtimeClass.getName}, ${ctr.runtimeClass.getName}]"))
      },
      Writes {
        case Right(x) => rFormat.writes(x)
        case Left(x) => lFormat.writes(x)
      }
    )

  // TODO create a function with f
  // create an implementation with formatValue
  val stringAsDoubleFormat: Format[Double] = Format(
    Reads.StringReads.map(_.toDouble),
    Writes.StringWrites.contramap[Double](formatValue(_))
  )

  implicit val assetDoubleMapFormat: Format[Map[Asset, Double]] = assetMapFormat[Double]

  implicit def byteStr58MapFormat[V: Format]: Format[Map[ByteStr, V]] = mapFormat[ByteStr, V](
    stringifyKey = _.base58,
    parseKey = x =>
      ByteStr.decodeBase58(x).fold[JsResult[ByteStr]](
        err => JsError(s"Can't parse '$x' as ByteStr, expected base58 string, error $err"),
        JsSuccess(_)
      )
  )

  implicit def assetMapFormat[V: Format]: Format[Map[Asset, V]] = mapFormat[Asset, V](
    stringifyKey = _.toString,
    parseKey = x => Asset.fromString(x).fold[JsResult[Asset]](JsError(s"Can't parse '$x' as Asset"))(JsSuccess(_))
  )

  implicit def assetPairMapFormat[V: Format]: Format[Map[AssetPair, V]] = mapFormat[AssetPair, V](
    stringifyKey = _.key,
    parseKey = x => {
      val assetPairStrArr = x.split("-")
      assetPairStrArr match {
        case Array(amtAssetStr, prcAssetStr) =>
          AssetPair
            .createAssetPair(amtAssetStr, prcAssetStr)
            .fold(e => JsError(s"Can't parse '$x' as AssetPair: ${e.getMessage}"), JsSuccess(_))
        case _ => JsError(s"$x (incorrect assets count, expected 2 but got ${assetPairStrArr.size})")
      }
    }
  )

  def mapFormat[K, V: Format](stringifyKey: K => String, parseKey: String => JsResult[K]): Format[Map[K, V]] = {
    val vReads = implicitly[Reads[V]]
    Format(
      fjs = Reads {
        case JsObject(xs) =>
          xs.foldLeft[JsResult[Map[K, V]]](JsSuccess(Map.empty[K, V])) {
            case (r, (k, v)) =>
              for {
                r <- r
                k <- parseKey(k)
                v <- vReads.reads(v)
              } yield r.updated(k, v)
          }
        case x => JsError(s"Can't parse map: $x")
      },
      tjs = Writes.map[V].contramap(_.map { case (k, v) => stringifyKey(k) -> v })
    )
  }

  implicit final class FormatOps[A](val self: Format[A]) extends AnyVal {
    def coerce[B](to: A => B, from: B => A): Format[B] = Format(self.map(to), self.contramap(from))
  }

  implicit val transactionFormat: Format[Transaction] = Format[Transaction](
    Reads { json =>
      Try(Transaction.fromJson(json.toString)) match {
        case Success(x) => JsSuccess(x)
        case Failure(e) => JsError(e.getMessage)
      }
    },
    Writes(tx => Json.parse(tx.toJson))
  )

  implicit val byteStrFormat: Format[ByteStr] = Format(
    Reads {
      case JsString(str) =>
        ByteStr.decodeBase58(str) match {
          case Success(x) => JsSuccess(x)
          case Failure(e) => JsError(e.getMessage)
        }

      case _ => JsError("Can't read ByteStr")
    },
    Writes(x => JsString(x.toString))
  )

  implicit val exchangeTxReads: Reads[ExchangeTransaction] = transactionFormat.map(_.asInstanceOf[ExchangeTransaction])

  implicit val orderWrites: Writes[Order] = Writes(_.json())

  implicit val assetPairFormat: Format[AssetPair] = AssetPair.assetPairFormat

  implicit val assetRatesReads: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { assetRates =>
      assetRates.map { case (assetStr, rateValue) => AssetPair.extractAsset(assetStr).get -> rateValue }
    }
  }

  implicit val assetBalancesReads: Reads[Map[Asset, Long]] = Reads.map[Long].map { assetBalances =>
    assetBalances.map { case (assetStr, balanceValue) => AssetPair.extractAsset(assetStr).get -> balanceValue }
  }

  implicit val assetPairOffsetsReads: Reads[Map[AssetPair, Long]] = Reads { json =>
    json.validate[Map[String, Long]].map {
      _.map {
        case (assetPairStr, offset) =>
          val assetPairStrArr = assetPairStr.split("-")
          val assetPair = (
            assetPairStrArr match {
              case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
              case _ => throw new Exception(s"$assetPairStr (incorrect assets count, expected 2 but got ${assetPairStrArr.size})")
            }
            ).fold(ex => throw new Exception(s"$assetPairStr (${ex.getMessage})"), identity)
          assetPair -> offset
      }
    }
  }

}
