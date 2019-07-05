package com.wavesplatform.dex

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json._
import shapeless._
import com.wavesplatform.dex.error.ErrorInterpolatorExtractor.{mk, mkCol, mkJsString}
import com.wavesplatform.dex.settings.DeviationsSettings
import com.wavesplatform.features.BlockchainFeature
import shapeless.ops.hlist.{Mapper, ToList}

package object error {
  implicit val byteExtractor               = mk[Byte](_.toString, x => JsNumber(x.toInt))
  implicit val intExtractor                = mk[Int](_.toString, JsNumber(_))
  implicit val longExtractor               = mk[Long](_.toString, JsNumber(_))
  implicit val doubleExtractor             = mk[Double](_.toString, JsNumber(_))
  implicit val stringExtractor             = mkJsString[String](identity)
  implicit val byteStrExtractor            = mkJsString[ByteStr](_.base58)
  implicit val assetExtractor              = mkJsString[Asset](AssetPair.assetIdStr)
  implicit val assetPairExtractor          = mk[AssetPair](_.key, _.json)
  implicit val publicKeyExtractor          = mkJsString[PublicKey](_.base58)
  implicit val addressExtractor            = mkJsString[Address](_.stringRepr)
  implicit val blockchainFeatureExtractor  = mkJsString[BlockchainFeature](_.description)
  implicit val deviationsSettingsExtractor = mkJsString[DeviationsSettings](_.toString) // todo

  implicit def setExtractor[T](implicit itemExtractor: ErrorInterpolatorExtractor[T]): ErrorInterpolatorExtractor[Set[T]]   = mkCol[Set, T](_.toList)
  implicit def listExtractor[T](implicit itemExtractor: ErrorInterpolatorExtractor[T]): ErrorInterpolatorExtractor[List[T]] = mkCol[List, T](_.toList)

  implicit def mapExtractor[K, V](implicit kExtractor: ErrorInterpolatorExtractor[K],
                                  vExtractor: ErrorInterpolatorExtractor[V]): ErrorInterpolatorExtractor[Map[K, V]] =
    new ErrorInterpolatorExtractor[Map[K, V]] {
      override def names(input: Map[K, V]): String = ""
      override def toString(input: Map[K, V]): String = {
        val xs = input.map {
          case (k, v) =>
            s"${kExtractor.toString(k)}: ${vExtractor.toString(v)}"
        }
        s"{${xs.mkString(", ")}}"
      }
      override def toJson(input: Map[K, V]): JsValue =
        JsObject(input.map {
          case (k, v) => kExtractor.toString(k) -> vExtractor.toJson(v)
        })
    }

  object ErrorInterpolatorMapper extends Poly1 {
    implicit def mapAt[T](implicit p: ErrorInterpolatorExtractor[T]): Case.Aux[T, (String, String, JsValue)] = at[T] { x =>
      (p.names(x), p.toString(x), p.toJson(x))
    }
  }

  implicit class ErrorInterpolator(sc: StringContext) {
    def e(): MatcherErrorMessage = {
      val x = sc.parts.head.trim
      MatcherErrorMessage(x, x, JsObject.empty)
    }

    def e[T](arg: (Symbol, T))(implicit extractor: ErrorInterpolatorExtractor[(Symbol, T)]): MatcherErrorMessage = {
      val name = extractor.names(arg)
      MatcherErrorMessage(
        s"${sc.parts.head}$name${sc.parts.last}".trim,
        s"${sc.parts.head}${extractor.toString(arg)}${sc.parts.last}".trim,
        Json.obj(name -> extractor.toJson(arg))
      )
    }

    def e[T, H <: HList, L <: HList](args: T)(implicit
                                              gen: Generic.Aux[T, H],
                                              mapper: Mapper.Aux[ErrorInterpolatorMapper.type, H, L],
                                              toList: ToList[L, Id[_]]): MatcherErrorMessage = {
      val parts = sc.parts.init

      val (nameArgs, strArgs, jsonArgs) = toList(mapper(gen.to(args))).asInstanceOf[List[(String, String, JsValue)]].unzip3
      val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
        case ((m, t, p), (x, i)) =>
          val name = nameArgs(i)
          val str  = strArgs(i)
          val json = jsonArgs(i)
          (s"$m$x$str", s"$t$x{{$name}}", p + (name -> json))
      }

      MatcherErrorMessage(
        (message + sc.parts.last).trim,
        (template + sc.parts.last).trim,
        params
      )
    }
  }
}
