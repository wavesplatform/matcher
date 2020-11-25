package com.wavesplatform.dex

import com.google.protobuf.ByteString
import com.softwaremill.diffx.scalatest.DiffMatcher
import com.softwaremill.diffx.{Derived, Diff, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

trait WavesIntegrationSuiteBase extends AnyFreeSpecLike with Matchers with DiffMatcher with AllureScalatestContext {

  implicit val derivedByteStrDiff: Derived[Diff[ByteStr]] = Derived(getDiff[ByteStr](_.toString == _.toString)) // TODO duplication
  implicit val derivedByteStringDiff: Derived[Diff[ByteString]] = Derived(getDiff[ByteString](_.toString == _.toString))
  implicit val derivedUtxTransactionDiff: Derived[Diff[UtxTransaction]] = Derived(getDiff[UtxTransaction](_.id == _.id))

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

}
