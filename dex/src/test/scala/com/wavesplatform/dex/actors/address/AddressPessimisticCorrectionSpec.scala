package com.wavesplatform.dex.actors.address

import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.actors.Generators
import com.wavesplatform.dex.domain.asset.Asset
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AddressPessimisticCorrectionSpec
    extends AnyFreeSpecLike
    with Generators
    with DiffMatcher
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  private val assetsMapGen = Gen.mapOf(Gen.zip(
    Gen.oneOf(definedAssetsGen, assetGen),
    Gen.choose(0L, 10L)
  ))

  "AddressPessimisticCorrection" - {
    "withProbablyStaleUnconfirmed - an update have the lower precedence unconfirmed" in forAll(assetsMapGen, assetsMapGen) {
      (unconfirmed, updates) =>
        val orig = AddressPessimisticCorrection(
          notObserved = Map.empty,
          unconfirmed = unconfirmed,
          future = Set.empty
        )

        val updated = orig.withInit(updates)
        updates.foreach { case (asset, v) =>
          updated.getBy(asset) shouldBe unconfirmed.getOrElse(asset, v)
        }
    }

    "withFreshUnconfirmed - an update have the higher precedence over unconfirmed" in forAll(assetsMapGen, assetsMapGen) {
      (unconfirmed, updates) =>
        val orig = AddressPessimisticCorrection(
          notObserved = Map.empty,
          unconfirmed = unconfirmed,
          future = Set.empty
        )

        val updated = orig.withFreshUnconfirmed(updates)
        updates.foreach { case (asset, v) =>
          updated.getBy(asset) shouldBe v
        }
    }

    "withExecuted" - {
      val stateGen = for {
        unconfirmed <- assetsMapGen // TODO should include from tx?
      } yield AddressPessimisticCorrection(Map.empty, Map.empty, Set.empty)

      "tx is either among notObserver or in future" in {}
      "reduces the unconfirmed volume" in {}
    }

    "withObserved" - {
      "tx is either among notObserver or in future" in {}
    }

    "getBy - takes into account a pessimistic correction by both unconfirmed and not observer" in {}
  }
}
