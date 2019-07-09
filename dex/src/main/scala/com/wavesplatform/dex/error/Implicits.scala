package com.wavesplatform.dex.error

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.error.ContextShow.{show, auto => autoShow}
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.settings.formatValue
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

object Implicits {
  // Here, because we doesn't want to leak this implicits outside the error package

  implicit val byteShow              = autoShow[Byte]
  implicit val intShow               = autoShow[Int]
  implicit val longShow              = autoShow[Long]
  implicit val stringShow            = show[String](identity)
  implicit val doubleShow            = stringShow.contramap[Double](formatValue)
  implicit val byteStrShow           = show[ByteStr](_.base58)
  implicit val assetShow             = show[Asset](AssetPair.assetIdStr)
  implicit val assetPairShow         = show[AssetPair](_.key)
  implicit val publicKeyShow         = show[PublicKey](_.base58)
  implicit val addressShow           = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow = show[BlockchainFeature](_.description)

  implicit val amountShow = new ContextShow[Amount] {
    override def show(input: Amount)(context: ErrorFormatterContext): String = {
      val denormalizedV = Denormalization.denormalizeAmountAndFee(input.volume, context.assetDecimals(input.asset).getOrElse(8))
      s"${formatValue(denormalizedV)} ${assetShow.show(input.asset)(context)}"
    }
  }

  implicit val priceShow = new ContextShow[Price] {
    override def show(input: Price)(context: ErrorFormatterContext): String =
      formatValue(
        Denormalization
          .denormalizePrice(
            input.volume,
            context.assetDecimals(input.assetPair.amountAsset).getOrElse(8),
            context.assetDecimals(input.assetPair.priceAsset).getOrElse(8)
          ))
  }

  implicit val balanceShow = new ContextShow[Map[Asset, Long]] {
    override def show(input: Map[Asset, Long])(context: ErrorFormatterContext): String =
      input.map(Function.tupled(Amount.apply)).map(amountShow.show(_)(context)).mkString(" and ")
  }

  implicit def setShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[Set[T]] {
    override def show(input: Set[T])(context: ErrorFormatterContext): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit def listShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[List[T]] {
    override def show(input: List[T])(context: ErrorFormatterContext): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit val booleanWrites           = ContextWrites.auto[Boolean]
  implicit val intWrites               = ContextWrites.auto[Int]
  implicit val byteWrites              = intWrites.contramap[Byte](_.toInt)
  implicit val longWrites              = ContextWrites.auto[Long]
  implicit val doubleWrites            = ContextWrites.auto[String].contramap[Double](formatValue)
  implicit val strWrites               = ContextWrites.auto[String]
  implicit val byteStrWrites           = strWrites.contramap[ByteStr](_.base58)
  implicit val assetWrites             = strWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = ContextWrites.writes[AssetPair]((x, _) => x.json)
  implicit val publicKeyWrites         = strWrites.contramap[PublicKey](_.base58)
  implicit val addressWrites           = strWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = strWrites.contramap[BlockchainFeature](_.description)

  implicit val amountWrites = new ContextWrites[Amount] {
    override def writes(input: Amount)(context: ErrorFormatterContext): JsValue = {
      val denormalizedV = Denormalization.denormalizeAmountAndFee(input.volume, context.assetDecimals(input.asset).getOrElse(8))
      Json.obj(
        "volume"  -> doubleWrites.writes(denormalizedV)(context),
        "assetId" -> assetWrites.writes(input.asset)(context)
      )
    }
  }

  implicit val balanceWrites = new ContextWrites[Map[Asset, Long]] {
    override def writes(input: Map[Asset, Long])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map {
        case (k, v) =>
          val denormalizedV = Denormalization.denormalizeAmountAndFee(v, context.assetDecimals(k).getOrElse(8))
          assetShow.show(k)(context) -> doubleWrites.writes(denormalizedV)(context)
      }
      JsObject(xs)
    }
  }

  implicit val priceWrites = new ContextWrites[Price] {
    override def writes(input: Price)(context: ErrorFormatterContext): JsValue =
      doubleWrites.writes(
        Denormalization
          .denormalizePrice(
            input.volume,
            context.assetDecimals(input.assetPair.amountAsset).getOrElse(8),
            context.assetDecimals(input.assetPair.priceAsset).getOrElse(8)
          )
      )(context)
  }

  // @TODO Make collection writes
  implicit def setWrites[T](implicit itemWrites: ContextWrites[T]) = new ContextWrites[Set[T]] {
    override def writes(input: Set[T])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map(itemWrites.writes(_)(context))
      implicitly[Writes[Set[JsValue]]].writes(xs)
    }
  }

  implicit def listWrites[T](implicit itemWrites: ContextWrites[T]) = new ContextWrites[List[T]] {
    override def writes(input: List[T])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map(itemWrites.writes(_)(context))
      implicitly[Writes[List[JsValue]]].writes(xs)
    }
  }

  implicit class ErrorInterpolator(sc: StringContext) {
    class Args extends ProductArgs {
      def applyProduct[H <: HList, L <: HList](args: H)(
          implicit
          formatArgs: Mapper.Aux[FormatArg.type, H, L],
          toList: ToList[L, Id[_]]
      ): ErrorFormatterContext => MatcherErrorMessage = {
        val (nameArgs, strArgsFn, jsonArgsFn) = toList(formatArgs(args))
          .asInstanceOf[List[(String, ErrorFormatterContext => String, ErrorFormatterContext => JsValue)]]
          .unzip3

        val parts = sc.parts.init

        { context =>
          val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
            case ((m, t, p), (x, i)) =>
              val name = nameArgs(i)
              val str  = strArgsFn(i)(context)
              val json = jsonArgsFn(i)(context)
              (s"$m$x$str", s"$t$x{{$name}}", p + (name -> json))
          }
          MatcherErrorMessage(
            normalize(message + sc.parts.last),
            normalize(template + sc.parts.last),
            params
          )
        }
      }
    }

    val e: Args = new Args

    def normalize(x: String): String = x.stripMargin('|').replaceAll("\n", " ").trim
  }

  object FormatArg extends Poly1 {
    implicit def mapAt[T](
        implicit show: ContextShow[T],
        json: ContextWrites[T]): Case.Aux[(Symbol, T), (String, ErrorFormatterContext => String, ErrorFormatterContext => JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x), json.writes(x))
      }
  }
}
