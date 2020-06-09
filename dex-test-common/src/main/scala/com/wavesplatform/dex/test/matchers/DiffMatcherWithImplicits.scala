package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.{Derived, Diff, DiffResultDifferent, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits.{DiffForMatcher, getDiff}
import org.scalatest.matchers.{MatchResult, Matcher}

trait DiffMatcherWithImplicits {

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]]     = Derived(getDiff[ByteStr](_.toString == _.toString))
  implicit val derivedPublicKeyDiff: Derived[Diff[PublicKey]] = Derived(derivedByteStrDiff.contramap[PublicKey](_.arr))

  def matchTo[A: Diff](left: A): DiffForMatcher[A] = DiffForMatcher(left)
}

object DiffMatcherWithImplicits {

  case class DiffForMatcher[A: Diff](right: A) extends Matcher[A] {
    override def apply(left: A): MatchResult = Diff[A].apply(left, right) match {
      case c: DiffResultDifferent =>
        val diff = c.show.split('\n').mkString(Console.RESET, s"${Console.RESET}\n${Console.RESET}", Console.RESET)
        MatchResult(matches = false, s"Matching error:\n$diff", "")
      case _ => MatchResult(matches = true, "", "")
    }
  }

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }
}
