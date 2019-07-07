package com.wavesplatform.dex.error

import cats.Show
import play.api.libs.json.{JsObject, JsValue, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

object Implicits {
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
