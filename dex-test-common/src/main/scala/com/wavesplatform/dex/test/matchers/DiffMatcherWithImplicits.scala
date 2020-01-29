package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr

trait DiffMatcherWithImplicits extends DiffMatcher {

  // https://github.com/softwaremill/diffx/issues/69
  // Sometimes it is useful, sometimes not.
//  import com.softwaremill.diffx.Diff._
//  implicit val multilineStringsDiff: Diff[String] =
//    diffForIterable[String, List](diffForOption(implicitly[Derived[Diff[String]]]))
//      .contramap[String](_.split("\n").toList)

  private val byteStrDiff: Diff[ByteStr] = (left: ByteStr, right: ByteStr, _: List[FieldPath]) => {
    if (left.toString == right.toString) Identical(left) else DiffResultValue(left, right)
  }

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(byteStrDiff)
}
