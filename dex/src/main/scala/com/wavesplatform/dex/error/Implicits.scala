package com.wavesplatform.dex.error

import cats.Show
import cats.Show.{show, fromToString => autoShow}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{JsObject, JsValue, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

object Implicits {
  // Here, because we doesn't want to leak this implicits outside the error package

  implicit val byteShow              = autoShow[Byte]
  implicit val intShow               = autoShow[Int]
  implicit val longShow              = autoShow[Long]
  implicit val doubleShow            = autoShow[Double]
  implicit val stringShow            = show[String](identity)
  implicit val byteStrShow           = show[ByteStr](_.base58)
  implicit val assetShow             = show[Asset](AssetPair.assetIdStr)
  implicit val issuedAssetShow       = show[IssuedAsset](AssetPair.assetIdStr)
  implicit val assetPairShow         = show[AssetPair](_.key)
  implicit val publicKeyShow         = show[PublicKey](_.base58)
  implicit val addressShow           = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow = show[BlockchainFeature](_.description)
  implicit val balanceShow = Show.show[Map[Asset, Long]] { input =>
    val xs = input.map { case (k, v) => s"${Show[Long].show(v)} ${Show[Asset].show(k)}" }
    s"${xs.mkString(" and ")}"
  }

  implicit def setShow[T](implicit itemShow: Show[T]): Show[Set[T]] =
    (input: Set[T]) => s"${input.map(itemShow.show).mkString(", ")}"

  implicit def listShow[T](implicit itemShow: Show[T]): Show[List[T]] =
    (input: List[T]) => s"${input.map(itemShow.show).mkString(", ")}"

  implicit val byteWrites              = Writes.IntWrites.contramap[Byte](_.toInt)
  implicit val byteStrWrites           = Writes.StringWrites.contramap[ByteStr](_.base58)
  implicit val assetWrites             = Writes.StringWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = Writes[AssetPair](_.json)
  implicit val publicKeyWrites         = Writes.StringWrites.contramap[PublicKey](_.base58)
  implicit val addressWrites           = Writes.StringWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = Writes.StringWrites.contramap[BlockchainFeature](_.description)
  implicit val balanceWrites           = mapWrites[Asset, Long]

  implicit def mapWrites[K, V](implicit kShow: Show[K], vWrites: Writes[V]): Writes[Map[K, V]] = { input =>
    val xs = input.map { case (k, v) => kShow.show(k) -> vWrites.writes(v) }
    JsObject(xs)
  }

  implicit class ErrorInterpolator(sc: StringContext) {
    class Args extends ProductArgs {
      def applyProduct[H <: HList, L <: HList](args: H)(
          implicit
          formatArgs: Mapper.Aux[FormatArg.type, H, L],
          toList: ToList[L, Id[_]]
      ): MatcherErrorMessage = {
        val parts = sc.parts.init

        val (nameArgs, strArgs, jsonArgs) = toList(formatArgs(args)).asInstanceOf[List[(String, String, JsValue)]].unzip3
        val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
          case ((m, t, p), (x, i)) =>
            val name = nameArgs(i)
            val str  = strArgs(i)
            val json = jsonArgs(i)
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
    implicit def mapAt[T](implicit show: Show[T], json: Writes[T]): Case.Aux[(Symbol, T), (String, String, JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x), json.writes(x))
      }
  }
}
