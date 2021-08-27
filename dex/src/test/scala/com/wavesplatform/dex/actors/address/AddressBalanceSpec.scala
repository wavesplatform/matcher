package com.wavesplatform.dex.actors.address

import cats.instances.long._
import cats.syntax.group._
import cats.syntax.option._
import com.softwaremill.diffx.scalatest.DiffMatcher
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.actors.Generators
import com.wavesplatform.dex.actors.address.AddressBalance.{NotCreatedTxData, NotObservedTxData}
import com.wavesplatform.dex.collections.{NegativeMap, NonNegativeMap, NonPositiveMap, PositiveMap}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AddressBalanceSpec
    extends AnyFreeSpecLike
    with Generators
    with DiffMatcher
    with DiffMatcherWithImplicits
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  private val positiveAssetsMapGen = assetsMapGen(Gen.choose(1L, 10))
  private val nonPositiveAssetsMapGen = assetsMapGen(Gen.choose(-10L, 0))
  private val negativeAssetsMapGen = assetsMapGen(Gen.choose(-10L, -1))

  private def assetsMapGen(valuesGen: Gen[Long]): Gen[Map[Asset, Long]] =
    Gen.mapOf(Gen.zip(Gen.oneOf(definedAssetsGen, assetGen), valuesGen))

  "AddressPessimisticCorrection" - {
    "common properties" - {
      "both notObservedTxs and notCreatedTxs doesn't contain same tx" in {
        val testGen = for {
          txId <- txIdGen
          txVolumeDiff <- negativeAssetsMapGen
          notCreatedTxs <- Gen.choose(0, 2).flatMap { n =>
            Gen.mapOfN(
              n,
              Gen.zip(
                txIdGen.filterNot(_ == txId),
                positiveAssetsMapGen.map(PositiveMap(_))
              )
            )
          }
          executedIsFirst <- Arbitrary.arbBool.arbitrary
        } yield {
          val orig = AddressBalance.empty.copy(
            // executed means we reserve it before
            reserved = if (executedIsFirst) PositiveMap(txVolumeDiff.inverse()) else PositiveMap.empty,
            notCreatedTxs = notCreatedTxs.view.mapValues(mkNotCreatedTxData).toMap
          )
          (orig, txId, NegativeMap(txVolumeDiff), executedIsFirst)
        }

        forAll(testGen) { case (orig, txId, txVolumeDiff, executedIsFirst) =>
          val updated =
            if (executedIsFirst) orig.withExecuted(txId.some, mkNotObservedTxData(txVolumeDiff))._1
            else orig.withObserved(txId, mkNotCreatedTxData(PositiveMap(txVolumeDiff.xs.view.mapValues(-_).toMap)))._1
          val notObserved = updated.notObservedTxs.contains(txId)
          val notCreated = updated.notCreatedTxs.contains(txId)

          val cond = !(notObserved && notCreated)
          withClue(s"notObserved: $notObserved, notCreatedTxs: $notCreated:") {
            cond shouldBe true
          }
        }
      }

      "the tradable balance changes only after both withExecuted and withObserved called, no matter in which order" in {
        val testGen = for {
          (orig, txId, txReserve) <- balanceAndTxGen
          executedIsFirst <- Arbitrary.arbBool.arbitrary
        } yield (orig, txId, txReserve, executedIsFirst)

        forAll(testGen) { case (orig, txId, txReserve, executedIsFirst) =>
          val txExecutedDiff = NegativeMap(txReserve.inverse())
          val updated1 =
            if (executedIsFirst) orig.withExecuted(txId.some, mkNotObservedTxData(txExecutedDiff))._1
            else orig.withObserved(txId, mkNotCreatedTxData(PositiveMap(txReserve)))._1
          updated1.allTradableBalance should matchTo(orig.allTradableBalance)

          val (updated2, diff) =
            if (executedIsFirst) updated1.withObserved(txId, mkNotCreatedTxData(PositiveMap(txReserve)))
            else updated1.withExecuted(txId.some, mkNotObservedTxData(txExecutedDiff))
          val actual = updated2.allTradableBalance.filter { case (asset, _) => diff.changedAssets.contains(asset) }

          val expected = orig.tradableBalance(txReserve.keySet).xs |+| txReserve
          actual should matchTo(expected)
        }
      }
    }

    "withExecuted" - {
      "reserves an execution amount" in forAll(balanceAndTxGen) { case (orig, _, txReserve) =>
        val (updated, affected) = orig.withExecuted(none, mkNotObservedTxData(NegativeMap(txReserve.inverse())))

        val actual = updated.reserved.filter { case (asset, _) => txReserve.contains(asset) }
        val expected = (orig.reserved.filter { case (asset, _) => txReserve.contains(asset) } |-| txReserve).filter { case (_, v) => v != 0 }
        withClue(s"$actual\nvs\n$expected") {
          actual should matchTo(expected)
        }

        affected.changedAssets should matchTo(txReserve.keySet)
      }

      "either" - {
        "if a tx isn't expected - does nothing" in forAll(balanceAndTxGen) { case (orig, _, txReserve) =>
          val (updated, affected) = orig.withExecuted(none, mkNotObservedTxData(NegativeMap(txReserve.inverse())))
          updated.copy(reserved = orig.reserved) should matchTo(orig)
          affected.changedAssets should matchTo(txReserve.keySet)
        }

        "if a tx is expected, either" - {
          "removes it from futureTxIds" in forAll(balanceAndTxGen) { case (orig, txId, txReserve) =>
            val (updated, affected) = orig
              .copy(notCreatedTxs = orig.notCreatedTxs.updated(txId, mkNotCreatedTxData(PositiveMap(txReserve))))
              .withExecuted(txId.some, mkNotObservedTxData(NegativeMap(txReserve.inverse())))
            updated.notCreatedTxs shouldNot contain(txId)
            updated.notObservedTxs.keySet shouldNot contain(txId)
            affected.changedAssets should matchTo(txReserve.keySet)
          }

          "adds it to notObservedTxs" in forAll(balanceAndTxGen) { case (orig, txId, txReserve) =>
            val (updated, affected) = orig.withExecuted(txId.some, mkNotObservedTxData(NegativeMap(txReserve.inverse())))
            updated.notObservedTxs.keySet should contain(txId)
            affected.changedAssets shouldBe empty
          }
        }
      }
    }

    "withObserved either" - {
      "removes an tx from the notObservedTxs map" in forAll(balanceAndTxGen) { case (orig, txId, txReserve) =>
        val (updated, affected) = orig
          .copy(notObservedTxs = orig.notObservedTxs.updated(txId, mkNotObservedTxData(NegativeMap(txReserve.inverse()))))
          .withObserved(txId, mkNotCreatedTxData(PositiveMap(txReserve)))
        updated.notCreatedTxs shouldNot contain(txId)
        updated.notObservedTxs.keySet shouldNot contain(txId)
        affected.changedAssets should matchTo(txReserve.keySet)
      }

      "adds a tx to futureTxIds" in forAll(balanceAndTxGen) { case (orig, txId, txReserve) =>
        val (updated, affected) = orig.withObserved(txId, mkNotCreatedTxData(PositiveMap(txReserve)))
        updated.notCreatedTxs.keySet should contain(txId)
        affected.changedAssets shouldBe empty
      }
    }
  }

  private def mkNotCreatedTxData(pessimisticChanges: PositiveMap[Asset, Long]): NotCreatedTxData =
    NotCreatedTxData(Seq.empty, pessimisticChanges)

  private def mkNotObservedTxData(executionTotalVolumeDiff: NegativeMap[Asset, Long]): NotObservedTxData =
    NotObservedTxData(Seq.empty, executionTotalVolumeDiff)

  private lazy val balanceAndTxGen = for {
    txId <- txIdGen
    regularBase <- positiveAssetsMapGen
    outgoingLeasing <- Gen.option(Gen.choose(0L, 10L))
    unconfirmed <- nonPositiveAssetsMapGen
    notCreatedTxs <- Gen.choose(0, 2).flatMap { n =>
      Gen.mapOfN(
        n,
        Gen.zip(
          txIdGen.filterNot(_ == txId),
          positiveAssetsMapGen.map(PositiveMap(_))
        )
      )
    }
    txReserve <- positiveAssetsMapGen
  } yield {
    val orig = AddressBalance.empty.copy(
      regular = NonNegativeMap(
        regularBase |+|
        txReserve |-|
        unconfirmed |+|
        outgoingLeasing.fold(Map.empty[Asset, Long])(x => Map(Asset.Waves -> x))
      ),
      outgoingLeasing = outgoingLeasing,
      reserved = PositiveMap(txReserve),
      unconfirmed = NonPositiveMap(unconfirmed),
      notCreatedTxs = notCreatedTxs.view.mapValues(mkNotCreatedTxData).toMap
    )
    (orig, txId, txReserve)
  }

}
