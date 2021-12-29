package com.wavesplatform.dex.it.test.matchers

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.util.Diffs

trait DiffMatcherWithImplicits extends DiffMatcher with Diffs {

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(getDiff[ByteStr](_.toString == _.toString))
  implicit val derivedPublicKeyDiff: Derived[Diff[PublicKey]] = Derived(derivedByteStrDiff.contramap[PublicKey](_.arr))

}

object DiffMatcherWithImplicits {

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

}
