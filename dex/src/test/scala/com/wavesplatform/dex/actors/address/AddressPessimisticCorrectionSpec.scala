//package com.wavesplatform.dex.actors.address
//
//import cats.instances.long._
//import cats.instances.map._
//import cats.syntax.semigroup._
//import cats.syntax.option._
//import com.softwaremill.diffx.scalatest.DiffMatcher
//import com.wavesplatform.dex.NoShrink
//import com.wavesplatform.dex.actors.Generators
//import com.wavesplatform.dex.collections.{NegativeMap, NonPositiveMap}
//import com.wavesplatform.dex.domain.asset.Asset
//import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
//import org.scalacheck.{Arbitrary, Gen}
//import org.scalatest.freespec.AnyFreeSpecLike
//import org.scalatest.matchers.should.Matchers
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//
//class AddressPessimisticCorrectionSpec
//    extends AnyFreeSpecLike
//    with Generators
//    with DiffMatcher
//    with Matchers
//    with ScalaCheckPropertyChecks
//    with NoShrink {
//
//  private val nonPositiveAssetsMapGen = assetsMapGen(Gen.choose(-10L, 0))
//  private val negativeAssetsMapGen = assetsMapGen(Gen.choose(-10L, -1))
//
//  private def assetsMapGen(valuesGen: Gen[Long]): Gen[Map[Asset, Long]] =
//    Gen.mapOf(Gen.zip(Gen.oneOf(definedAssetsGen, assetGen), valuesGen))
//
//  "AddressPessimisticCorrection" - {
//    "common properties" - {
//      val testGen = for {
//        txId <- txIdGen
//        txVolumeDiff <- negativeAssetsMapGen
//        unconfirmed <- nonPositiveAssetsMapGen
//        future <- Gen.choose(0, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen.filterNot(_ == txId)))
//        executedIsFirst <- Arbitrary.arbBool.arbitrary
//      } yield {
//        val orig = AddressPessimisticCorrection.empty.copy(unconfirmed = NonPositiveMap(unconfirmed |+| txVolumeDiff), futureTxIds = future)
//        (orig, txId, NegativeMap(txVolumeDiff), executedIsFirst)
//      }
//
//      "notObservedTxs and futureTxIds doesn't contain tx in same time" in forAll(testGen) { case (orig, txId, txVolumeDiff, executedIsFirst) =>
//        val updated = if (executedIsFirst) orig.withExecuted(txId.some, txVolumeDiff)._1 else orig.withObserved(txId)._1
//        val notObserved = updated.notObservedTxs.contains(txId)
//        val futureTxIds = updated.futureTxIds.contains(txId)
//
//        val cond = !(notObserved && futureTxIds)
//        withClue(s"notObserved: $notObserved, futureTxIds: $futureTxIds:") {
//          cond shouldBe true
//        }
//      }
//    }
//
//    "withInitUnconfirmed" - {
//      "doesn't affect other assets" in unconfirmedTest { (orig, unconfirmed, updates) =>
//        val updated = orig.withInitUnconfirmed(updates)
//        (unconfirmed.xs.keySet -- updates.xs.keys).foreach { asset =>
//          updated.getBy(asset) shouldBe unconfirmed.getOrElse(asset, 0L)
//        }
//      }
//
//      "an update have the lower precedence unconfirmed" in unconfirmedTest { (orig, unconfirmed, updates) =>
//        val updated = orig.withInitUnconfirmed(updates)
//        updates.foreach { case (asset, v) =>
//          updated.getBy(asset) shouldBe unconfirmed.getOrElse(asset, v)
//        }
//      }
//    }
//
//    "withFreshUnconfirmed" - {
//      "doesn't affect other assets" in unconfirmedTest { (orig, unconfirmed, updates) =>
//        val updated = orig.withFreshUnconfirmed(updates)
//        (unconfirmed.xs.keySet -- updates.xs.keys).foreach { asset =>
//          updated.getBy(asset) shouldBe unconfirmed.getOrElse(asset, 0L)
//        }
//      }
//
//      "an update have the higher precedence over unconfirmed" in unconfirmedTest { (orig, _, updates) =>
//        val updated = orig.withFreshUnconfirmed(updates)
//        updates.foreach { case (asset, v) =>
//          updated.getBy(asset) shouldBe v
//        }
//      }
//    }
//
//    "withExecuted either" - {
//      "if a tx isn't expected - does nothing" in {
//        val stateGen = for {
//          unconfirmed <- nonPositiveAssetsMapGen
//          futureTxIds <- Gen.choose(0, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen))
//          txVolumeDiff <- negativeAssetsMapGen
//        } yield {
//          val init = AddressPessimisticCorrection.empty.copy(unconfirmed = NonPositiveMap(unconfirmed |+| txVolumeDiff), futureTxIds = futureTxIds)
//          (init, NegativeMap(txVolumeDiff))
//        }
//
//        forAll(stateGen) { case (orig, txVolumeDiff) =>
//          val (updated, affected) = orig.withExecuted(none, txVolumeDiff)
//          updated should matchTo(orig)
//          affected should matchTo(txVolumeDiff.xs.keySet)
//        }
//      }
//
//      "if a tx is expected, either" - {
//        "removes it from futureTxIds" in {
//          val stateGen = for {
//            unconfirmed <- nonPositiveAssetsMapGen
//            futureTxIds <- Gen.choose(1, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen))
//            txId <- Gen.oneOf(futureTxIds)
//            txVolumeDiff <- negativeAssetsMapGen
//          } yield {
//            val init =
//              AddressPessimisticCorrection.empty.copy(unconfirmed = NonPositiveMap(unconfirmed |+| txVolumeDiff), futureTxIds = futureTxIds)
//            (init, txId, NegativeMap(txVolumeDiff))
//          }
//
//          forAll(stateGen) { case (orig, txId, txVolumeDiff) =>
//            val (updated, affected) = orig.withExecuted(txId.some, txVolumeDiff)
//            updated.futureTxIds shouldNot contain(txId)
//            affected should matchTo(txVolumeDiff.xs.keySet)
//          }
//        }
//
//        "adds it to notObservedTxs" in {
//          val stateGen = for {
//            unconfirmed <- nonPositiveAssetsMapGen
//            futureTxIds <- Gen.choose(0, 2).flatMap(Gen.containerOfN[Set, ExchangeTransaction.Id](_, txIdGen))
//            txId <- txIdGen.filterNot(futureTxIds.contains)
//            txVolumeDiff <- negativeAssetsMapGen
//          } yield {
//            val init = AddressPessimisticCorrection.empty.copy(unconfirmed = NonPositiveMap(unconfirmed |+| txVolumeDiff), futureTxIds = futureTxIds)
//            (init, txId, NegativeMap(txVolumeDiff))
//          }
//
//          forAll(stateGen) { case (orig, txId, txVolumeDiff) =>
//            val (updated, affected) = orig.withExecuted(txId.some, txVolumeDiff)
//            updated.notObservedTxs.keySet should contain(txId)
//            affected shouldBe empty // Because
//          }
//        }
//      }
//    }
//
//    "withObserved either" - {
//      "removes an tx observed previously" ignore {}
//      "adds a tx to futureTxIds" ignore {}
//    }
//
//    "getBy - takes into account a pessimistic correction by both unconfirmed and not observer" ignore {}
//  }
//
//  private def unconfirmedTest(f: (AddressPessimisticCorrection, NonPositiveMap[Asset, Long], NonPositiveMap[Asset, Long]) => Unit): Unit =
//    forAll(nonPositiveAssetsMapGen, nonPositiveAssetsMapGen) {
//      (unconfirmed, updates) =>
//        val unconfirmedTyped = NonPositiveMap(unconfirmed)
//        val orig = AddressPessimisticCorrection(
//          notObservedTxs = Map.empty,
//          unconfirmed = unconfirmedTyped,
//          futureTxIds = Set.empty
//        )
//
//        f(orig, unconfirmedTyped, NonPositiveMap(updates))
//    }
//
//}
