package com.wavesplatform.dex.error

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{JsObject, JsValue, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

trait Context

trait ContextShow[-T] {
  def show(input: T)(context: Context): String
}

object ContextShow {
  def apply[T](implicit r: ContextShow[T]): ContextShow[T] = r

  def auto[T]: ContextShow[T] = show(_.toString)

  def show[T](f: T => String): ContextShow[T] = new ContextShow[T] {
    override def show(input: T)(context: Context): String = f(input)
  }
}

trait ContextWrites[-T] {
  def writes(input: T)(context: Context): JsValue
}

import ContextShow.{show, auto => autoShow}

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
  implicit val balanceShow = new ContextShow[Map[Asset, Long]] {
    override def show(input: Map[Asset, Long])(context: Context): String = {
      val xs = input.map { case (k, v) => s"${ContextShow[Long].show(v)(context)} ${ContextShow[Asset].show(k)(context)}" }
      s"${xs.mkString(" and ")}"
    }
  }

  implicit def setShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[Set[T]] {
    override def show(input: Set[T])(context: Context): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit def listShow[T](implicit itemShow: ContextShow[T]) = new ContextShow[List[T]] {
    override def show(input: List[T])(context: Context): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit val byteWrites              = Writes.IntWrites.contramap[Byte](_.toInt)
  implicit val byteStrWrites           = Writes.StringWrites.contramap[ByteStr](_.base58)
  implicit val assetWrites             = Writes.StringWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = Writes[AssetPair](_.json)
  implicit val publicKeyWrites         = Writes.StringWrites.contramap[PublicKey](_.base58)
  implicit val addressWrites           = Writes.StringWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = Writes.StringWrites.contramap[BlockchainFeature](_.description)
  implicit val balanceWrites           = mapWrites[Asset, Long]

  implicit def mapWrites[K, V](implicit kShow: ContextShow[K], vWrites: ContextWrites[V]) = new ContextWrites[Map[K, V]] {
    override def writes(input: Map[K, V])(context: Context): JsValue = {
      val xs = input.map { case (k, v) => kShow.show(k)(context) -> vWrites.writes(v)(context) }
      JsObject(xs)
    }
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
    // TODO
    implicit def mapAt[T](implicit show: ContextShow[T], json: ContextWrites[T]): Case.Aux[(Symbol, T), (String, Context => String, Context => JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x)(_), json.writes(x)(_))
      }
  }
}
