package com.wavesplatform.dex.test.matchers

import com.softwaremill.diffx.{Derived, Diff, DiffResultDifferent, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits.DiffForMatcher
import org.scalatest.matchers.{MatchResult, Matcher}

trait DiffMatcherWithImplicits {

  private val byteStrDiff: Diff[ByteStr] = (left: ByteStr, right: ByteStr, _: List[FieldPath]) => {
    if (left.toString == right.toString) Identical(left) else DiffResultValue(left, right)
  }

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
}
