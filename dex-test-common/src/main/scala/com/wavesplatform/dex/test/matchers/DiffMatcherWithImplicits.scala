package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr

trait DiffMatcherWithImplicits extends DiffMatcher {

  private val byteStrDiff: Diff[ByteStr] = (left: ByteStr, right: ByteStr, _: List[FieldPath]) => {
    if (left.toString == right.toString) Identical(left) else DiffResultValue(left, right)
  }

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(byteStrDiff)
}
