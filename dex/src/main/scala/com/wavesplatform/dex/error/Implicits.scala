package com.wavesplatform.dex.error

import cats.Show._
import cats.instances.list._
import cats.syntax.contravariant._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.show._
import cats.{Show, Traverse}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.settings.formatValue
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

object Implicits {

  // Here, because we doesn't want to leak this implicits outside the error package

  implicit val byteShow   = cats.instances.byte.catsStdShowForByte
  implicit val intShow    = cats.instances.int.catsStdShowForInt
  implicit val longShow   = cats.instances.long.catsStdShowForLong
  implicit val stringShow = cats.instances.string.catsStdShowForString

  implicit val doubleShow            = show[Double]((d: Double) => formatValue(d))
  implicit val byteStrShow           = show[ByteStr](_.toString)
  implicit val assetShow             = show[Asset](AssetPair.assetIdStr)
  implicit val issuedAssetShow       = show[IssuedAsset](AssetPair.assetIdStr)
  implicit val wavesShow             = show[Waves.type](_ => "WAVES")
  implicit val assetPairShow         = show[AssetPair](_.key)
  implicit val publicKeyShow         = show[PublicKey](_.toString)
  implicit val addressShow           = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow = show[BlockchainFeature](_.description)
  implicit val amountShow            = show[Amount](amount => s"${formatValue(amount.volume)} ${assetShow show amount.asset}")
  implicit val priceShow             = show[Price](input => formatValue(input.volume))
  implicit val balanceShow           = show[List[Amount]] { _.map(amountShow.show).mkString(" and ") }

  implicit def traverseShow[F[_]: Traverse, T: Show]: Show[F[T]] = { traverse: F[T] =>
    s"${traverse.map(_.show).mkString_(", ")}"
  }

  implicit def listShow[T: Show]: Show[List[T]] = traverseShow[List, T]
  implicit def setShow[T: Show]: Show[Set[T]]   = listShow[T].contramap[Set[T]](_.toList)

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

  implicit def setWrites[T](implicit itemWrites: ContextWrites[T]): ContextWrites[Set[T]] = (input: Set[T]) => {
    val xs = input.map(itemWrites.writes)
    implicitly[Writes[Set[JsValue]]].writes(xs)
  }

  implicit def listWrites[T](implicit itemWrites: ContextWrites[T]): ContextWrites[List[T]] = (input: List[T]) => {
    val xs = input.map(itemWrites.writes)
    implicitly[Writes[List[JsValue]]].writes(xs)
  }

  implicit class ErrorInterpolator(sc: StringContext) {

    class Args extends ProductArgs {
      def applyProduct[H <: HList, L <: HList](args: H)(
          implicit
          formatArgs: Mapper.Aux[FormatArg.type, H, L],
          toList: ToList[L, Id[_]]
      ): MatcherErrorMessage = {

        val (nameArgs, strArgsFn, jsonArgsFn) =
          toList { formatArgs(args) }
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
    implicit def mapAt[T: Show](implicit json: ContextWrites[T]): Case.Aux[(Symbol, T), (String, String, JsValue)] =
      at[(Symbol, T)] { case (name, x) => (name.name, x.show, json.writes(x)) }
  }
}
