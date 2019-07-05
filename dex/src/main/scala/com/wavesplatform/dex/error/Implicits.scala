package com.wavesplatform.dex.error

import cats.Show
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{Generic, HList, Id, Poly1}

object Implicits {
  implicit class ErrorInterpolator(sc: StringContext) {
    def e(): MatcherErrorMessage = {
      val x = sc.parts.head.trim
      MatcherErrorMessage(x, x, JsObject.empty)
    }

    def e[T](arg: (Symbol, T))(implicit show: Show[T], json: Writes[T]): MatcherErrorMessage = {
      val name = arg._1.name
      MatcherErrorMessage(
        normalize(s"${sc.parts.head}$name${sc.parts.last}"),
        normalize(s"${sc.parts.head}${show.show(arg._2)}${sc.parts.last}"),
        Json.obj(name -> json.writes(arg._2))
      )
    }

    def e[T, H <: HList, L <: HList](args: T)(implicit
                                              gen: Generic.Aux[T, H],
                                              formatArgs: Mapper.Aux[FormatArg.type, H, L],
                                              toList: ToList[L, Id[_]]): MatcherErrorMessage = {
      val parts = sc.parts.init

      val (nameArgs, strArgs, jsonArgs) = toList(formatArgs(gen.to(args))).asInstanceOf[List[(String, String, JsValue)]].unzip3
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

    def normalize(x: String): String = x.stripMargin('|').replaceAll("\n", " ").trim
  }

  object FormatArg extends Poly1 {
    implicit def mapAt[T](implicit show: Show[T], json: Writes[T]): Case.Aux[(Symbol, T), (String, String, JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x), json.writes(x))
      }
  }
}
