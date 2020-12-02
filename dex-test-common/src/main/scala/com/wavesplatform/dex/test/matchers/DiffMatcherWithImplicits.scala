package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits.getDiff

trait DiffMatcherWithImplicits extends DiffMatcher {

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(getDiff[ByteStr](_.toString == _.toString))
  implicit val derivedPublicKeyDiff: Derived[Diff[PublicKey]] = Derived(derivedByteStrDiff.contramap[PublicKey](_.arr))

}

object DiffMatcherWithImplicits {

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

}
