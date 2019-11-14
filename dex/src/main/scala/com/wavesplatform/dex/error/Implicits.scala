package com.wavesplatform.dex.error

import cats.syntax.contravariant._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.error.ContextShow.{show, auto => autoShow}
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
  implicit val doubleShow            = stringShow.contramap[Double]((x: Double) => formatValue(x))
  implicit val byteStrShow           = show[ByteStr](_.toString)
  implicit val assetShow             = show[Asset](AssetPair.assetIdStr)
  implicit val assetPairShow         = show[AssetPair](_.key)
  implicit val publicKeyShow         = show[PublicKey](_.toString)
  implicit val addressShow           = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow = show[BlockchainFeature](_.description)

  implicit val amountShow = new ContextShow[Amount] {
    override def show(input: Amount): String = s"${formatValue(input.volume)} ${assetShow.show(input.asset)}"
  }

  implicit val priceShow = new ContextShow[Price] {
    override def show(input: Price): String = formatValue(input.volume)
  }

  implicit val balanceShow = new ContextShow[List[Amount]] {
    override def show(input: List[Amount]): String = input.map(amountShow.show).mkString(" and ")
  }

  implicit def setShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[Set[T]] {
    override def show(input: Set[T]): String = s"${input.map(itemShow.show).mkString(", ")}"
  }

  implicit def listShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[List[T]] {
    override def show(input: List[T]): String = s"${input.map(itemShow.show).mkString(", ")}"
  }

  implicit val booleanWrites           = ContextWrites.auto[Boolean]
  implicit val intWrites               = ContextWrites.auto[Int]
  implicit val byteWrites              = intWrites.contramap[Byte](_.toInt)
  implicit val longWrites              = ContextWrites.auto[Long]
  implicit val doubleWrites            = ContextWrites.auto[String].contramap[Double](x => formatValue(BigDecimal(x)))
  implicit val decimalWrites           = ContextWrites.auto[String].contramap[BigDecimal](formatValue)
  implicit val strWrites               = ContextWrites.auto[String]
  implicit val byteStrWrites           = strWrites.contramap[ByteStr](_.toString)
  implicit val assetWrites             = strWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = ContextWrites.contextWrites[AssetPair](_.json)
  implicit val publicKeyWrites         = strWrites.contramap[PublicKey](_.toString)
  implicit val addressWrites           = strWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = strWrites.contramap[BlockchainFeature](_.description)

  implicit val amountWrites = new ContextWrites[Amount] {
    override def writes(input: Amount): JsValue =
      Json.obj(
        "volume"  -> Json.toJson(input.volume),
        "assetId" -> Json.toJson(input.asset)
      )
  }

  implicit val balanceWrites = new ContextWrites[List[Amount]] {
    override def writes(input: List[Amount]): JsValue = {
      val xs = input.map { x =>
        assetShow.show(x.asset) -> Json.toJson(x.volume)
      }
      JsObject(xs)
    }
  }

  implicit val priceWrites = decimalWrites.contramap[Price](_.volume)

  implicit def setWrites[T](implicit itemWrites: ContextWrites[T]) = new ContextWrites[Set[T]] {
    override def writes(input: Set[T]): JsValue = {
      val xs = input.map(itemWrites.writes)
      implicitly[Writes[Set[JsValue]]].writes(xs)
    }
  }

  implicit def listWrites[T](implicit itemWrites: ContextWrites[T]) = new ContextWrites[List[T]] {
    override def writes(input: List[T]): JsValue = {
      val xs = input.map(itemWrites.writes)
      implicitly[Writes[List[JsValue]]].writes(xs)
    }
  }

  implicit class ErrorInterpolator(sc: StringContext) {
    class Args extends ProductArgs {
      def applyProduct[H <: HList, L <: HList](args: H)(
          implicit
          formatArgs: Mapper.Aux[FormatArg.type, H, L],
          toList: ToList[L, Id[_]]
      ): MatcherErrorMessage = {
        val (nameArgs, strArgsFn, jsonArgsFn) = toList(formatArgs(args))
          .asInstanceOf[List[(String, String, JsValue)]]
          .unzip3

        val parts = sc.parts.init

        val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
          case ((m, t, p), (x, i)) =>
            val name = nameArgs(i)
            val str  = strArgsFn(i)
            val json = jsonArgsFn(i)
            (s"$m$x$str", s"$t$x{{$name}}", p + (name -> json))
        }
        MatcherErrorMessage(
          normalize(message + sc.parts.last),
          normalize(template + sc.parts.last),
          params
        )
      }
    }

    val e: Args = new Args

    def normalize(x: String): String = x.stripMargin('|').replaceAll("\n", " ").trim
  }

  object FormatArg extends Poly1 {
    implicit def mapAt[T](implicit show: ContextShow[T], json: ContextWrites[T]): Case.Aux[(Symbol, T), (String, String, JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x), json.writes(x))
      }
  }
}
