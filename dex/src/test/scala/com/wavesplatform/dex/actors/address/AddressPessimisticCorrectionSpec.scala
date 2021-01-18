package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.instances.map._
import cats.syntax.semigroup._
import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.actors.Generators
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import org.scalacheck.{Arbitrary, Gen}
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
    "common properties" - {
      val testGen = for {
        txId <- txIdGen
        txVolume <- assetsMapGen
        unconfirmed <- assetsMapGen
        future <- Gen.choose(0, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen.filterNot(_ == txId)))
        observedCalls <- Gen.choose(0, 2)
        executedCallPos <- Gen.option(Gen.choose(0, observedCalls))
      } yield {
        val orig = AddressPessimisticCorrection.empty.copy(unconfirmed = unconfirmed |+| txVolume, future = future)
        val c1 = List.fill(observedCalls)("withObserved")
        val calls = executedCallPos match {
          case None => c1
          case Some(pos) =>
            val (left, right) = c1.splitAt(pos)
            left.appended("withExecuted") ::: right
        }
        (orig, txId, txVolume, calls)
      }

      "notObserved and future doesn't contain tx in same time" in forAll(testGen) { case (orig, txId, txVolume, calls) =>
        val updated = calls.foldLeft(orig) {
          case (orig, "withObserved") => orig.withObserved(txId)._1
          case (orig, "withExecuted") => orig.withExecuted(txId, txVolume)._1
          case (orig, _) => orig
        }

        val notObserved = updated.notObserved.contains(txId)
        val future = updated.future.contains(txId)

        val cond = !(notObserved && future)
        withClue(s"notObserved: $notObserved, future: $future:") {
          cond shouldBe true
        }
      }

      // invariant?
      "correction doesn't change when withObserved, withExecuted" ignore forAll(testGen) { case (orig, txId, txVolume, calls) =>
        val updated = calls.foldLeft(orig) {
          case (orig, "withObserved") => orig.withObserved(txId)._1
          case (orig, "withExecuted") => orig.withExecuted(txId, txVolume)._1
          case (orig, _) => orig
        }

//        (orig.unconfirmed.keySet ++ txVolume.keySet).foreach { asset =>
//          orig.getBy(asset) shouldBe
//        }
      }
    }

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
        unconfirmed <- assetsMapGen
        future <- Gen.choose(0, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen))
        txId <- txIdGen
        txVolume <- assetsMapGen
        observed <- Arbitrary.arbBool.arbitrary
      } yield {
        val init = AddressPessimisticCorrection.empty.copy(unconfirmed = unconfirmed, future = future)
        val orig = if (observed) init.withObserved(txId)._1 else init
        (orig, txId, txVolume, orig.withExecuted(txId, txVolume)._1)
      }

      "future doesn't contain txId" in forAll(stateGen) { case (_, txId, _, updated) =>
        updated.future shouldNot contain(txId)
      }

      "reduces the unconfirmed volume" in forAll(stateGen) { case (orig, txId, txVolume, updated) =>
        orig.unconfirmed.filterNot(_._2 == 0) should matchTo((updated.unconfirmed |+| txVolume).filterNot(_._2 == 0))
      }
    }

    "withObserved" - {
      "tx is either among notObserver or in future" ignore {}
    }

    "getBy - takes into account a pessimistic correction by both unconfirmed and not observer" ignore {}
  }
}
