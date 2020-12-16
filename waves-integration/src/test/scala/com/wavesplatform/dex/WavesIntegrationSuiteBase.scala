package com.wavesplatform.dex

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.softwaremill.diffx.{ConsoleColorConfig, Derived, Diff, DiffResultDifferent, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.enablers.Emptiness
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

// TODO DEX-994
trait WavesIntegrationSuiteBase extends AnyFreeSpecLike with Matchers with AllureScalatestContext {

  // scalatest

  implicit val optionEmptiness: Emptiness[Option[Any]] = (thing: Option[Any]) => thing.isEmpty

  // diffx

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(getDiff[ByteStr](_.toString == _.toString)) // TODO duplication
  implicit val derivedByteStringDiff: Derived[Diff[ByteString]] = Derived(getDiff[ByteString](_.toString == _.toString))
  implicit val derivedUtxTransactionDiff: Derived[Diff[UtxTransaction]] = Derived(getDiff[UtxTransaction](_.id == _.id))

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

  def matchTo[A: Diff](right: A)(implicit c: ConsoleColorConfig): Matcher[A] = { left =>
    Diff[A].apply(left, right) match {
      case c: DiffResultDifferent =>
        val diff = c.show.split('\n').mkString(Console.RESET, s"${Console.RESET}\n${Console.RESET}", Console.RESET)
        MatchResult(matches = false, s"Matching error:\n$diff\nleft: $left", "")
      case _ => MatchResult(matches = true, "", "")
    }
  }

  protected def mkTxId(n: Int): ByteString = {
    require(n <= 127) // or we need complex implementation
    UnsafeByteOperations.unsafeWrap(new Array[Byte](n))
  }

}
