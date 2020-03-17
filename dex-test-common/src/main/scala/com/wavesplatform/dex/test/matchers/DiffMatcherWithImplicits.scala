package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.{Derived, Diff, DiffResultDifferent, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits.{DiffForMatcher, getDiff}
import org.scalatest.matchers.{MatchResult, Matcher}

trait DiffMatcherWithImplicits {

  private val byteStrDiff: Diff[ByteStr] = getDiff[ByteStr](_.toString == _.toString)

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(byteStrDiff)

  def matchTo[A: Diff](left: A): DiffForMatcher[A] = DiffForMatcher(left)
}

object DiffMatcherWithImplicits {

  case class DiffForMatcher[A: Diff](right: A) extends Matcher[A] {
    override def apply(left: A): MatchResult = Diff[A].apply(left, right) match {
      case c: DiffResultDifferent => MatchResult(matches = false, s"Matching error:\n${Console.RESET}${c.show}${Console.RESET}", "")
      case _                      => MatchResult(matches = true, "", "")
    }
  }

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }
}
