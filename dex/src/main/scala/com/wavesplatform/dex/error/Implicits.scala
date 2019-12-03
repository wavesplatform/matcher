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

  implicit val doubleShow            = show[Double](d => formatValue(d))
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

  implicit val byteWrites    = Writes.ByteWrites
  implicit val intWrites     = Writes.IntWrites
  implicit val longWrites    = Writes.LongWrites
  implicit val stringWrites  = Writes.StringWrites
  implicit val booleanWrites = Writes.BooleanWrites

  implicit val doubleWrites            = stringWrites.contramap[Double](d => formatValue(d))
  implicit val decimalWrites           = stringWrites.contramap[BigDecimal](formatValue)
  implicit val byteStrWrites           = stringWrites.contramap[ByteStr](_.toString)
  implicit val assetWrites             = stringWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites         = Writes[AssetPair](_.json)
  implicit val publicKeyWrites         = stringWrites.contramap[PublicKey](_.toString)
  implicit val addressWrites           = stringWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites = stringWrites.contramap[BlockchainFeature](_.description)

  implicit val amountWrites = Writes[Amount] { amount: Amount =>
    Json.obj("volume" -> Json.toJson(amount.volume), "assetId" -> Json.toJson(amount.asset))
  }

  implicit val balanceWrites = Writes[List[Amount]] { balance: List[Amount] =>
    JsObject(
      balance.map { amount =>
        assetShow.show(amount.asset) -> Json.toJson(amount.volume)
      }
    )
  }

  implicit val priceWrites = decimalWrites.contramap[Price](_.volume)

  implicit def listWrites[T: Writes]: Writes[List[T]] = Writes.traversableWrites[T].contramap[List[T]](_.toTraversable)
  implicit def setWrites[T: Writes]: Writes[Set[T]]   = listWrites[T].contramap[Set[T]](_.toList)

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
    implicit def mapAt[T: Show: Writes]: Case.Aux[(Symbol, T), (String, String, JsValue)] = at[(Symbol, T)] {
      case (name, arg) => (name.name, arg.show, implicitly[Writes[T]] writes arg)
    }
  }
}
