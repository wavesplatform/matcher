package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}
import shapeless.ops.hlist.{Mapper, ToTraversable, Zip}
import shapeless.{::, Generic, HList, HNil, Poly1}

// TODO use ApiError instead
case class MatcherError(error: Int, message: String, status: String, params: Option[MatcherError.Params])

object MatcherError {
  implicit val format: Format[MatcherError] = Json.format[MatcherError]

  case class Params(assetId: Option[String] = None, address: Option[String] = None, insignificantDecimals: Option[Int] = None) {
    def isEmpty: Boolean = assetId.isEmpty && address.isEmpty
  }

  object Params {
    implicit val format: Format[Params] = Json.format[Params]

    private object containsPoly extends Poly1 {

      implicit def contains[T: Ordering] = at[(Option[T], Option[T])] {
        case (Some(l), Some(r)) => l == r
        case (None, None) => true
        case _ => false
      }

    }

    private def internalContains[HParams <: HList, ZHParams <: HList, Booleans <: HList](obj: Params, part: Params)(
      implicit
      gen: Generic.Aux[Params, HParams],
      zip: Zip.Aux[HParams :: HParams :: HNil, ZHParams],
      mapper: Mapper.Aux[containsPoly.type, ZHParams, Booleans],
      toList: ToTraversable.Aux[Booleans, List, Boolean]
    ): Boolean =
      gen.to(obj).zip(gen.to(part))(zip).map(containsPoly).toList[Boolean](toList).forall(identity)

    def contains(obj: Params, part: Params): Boolean = internalContains(obj, part)
  }

}
