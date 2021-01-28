package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.instances.list._
import cats.instances.long._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.foldable._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

abstract class PessimisticPortfoliosTestSuiteBase
    extends WavesIntegrationSuiteBase
    with PBEntitiesGen
    with ScalaCheckDrivenPropertyChecks
    with NoShrink {

  val addresses: List[Address] = Gen.listOfN(3, addressGen).sample.get
  val limitedAddressGen: Gen[Address] = Gen.oneOf(addresses)
  val limitedAssetsGen: Gen[Asset] = Gen.oneOf(Asset.Waves, issuedAssetGen.sample.get)

  val addressPessimisticPortfolioGen: Gen[(Address, Map[Asset, Long])] = for {
    address <- limitedAddressGen
    assets <- Gen.mapOf(Gen.zip(limitedAssetsGen, Gen.choose(-100L, -1L)))
  } yield (address, assets)

  val pessimisticPortfolioGen: Gen[AddressAssets] = Gen.mapOf(addressPessimisticPortfolioGen)

  val pessimisticTransactionGen: Gen[PessimisticTransaction] = for {
    id <- pbTxIdGen
    pp <- pessimisticPortfolioGen
  } yield PessimisticTransaction(id, pp)

  val pessimisticPortfoliosGen: Gen[PessimisticPortfolios] = Gen.listOf(pessimisticTransactionGen).map(mkPessimisticPortfolios)

  def defaultBehaviorTests(): Unit = {
    // "getAggregated" - {} // used in consecutive tests

    "replaceWith" - {
      // TODO DEX-1013 Test results
      "whatever a state, the new state should be equal to arguments" - {
        val testGen = Gen.zip(pessimisticPortfoliosGen, Gen.listOf(pessimisticTransactionGen))

        "in general" in forAll(testGen) { case (pp, arg) =>
          pp.replaceWith(arg)
          val expected = collectChanges(arg)
          filterAddressesBy(getState(pp), expected) should matchTo(expected)
        }

        "for each address" in forAll(testGen) { case (pp, arg) =>
          pp.replaceWith(arg)

          val expectedState = arg.foldMap(_.pessimisticPortfolio)
          expectedState.foreach { case (address, expectedPP) =>
            withClue(s"$address: ") {
              filterAssetsBy(pp.getAggregated(address), expectedPP) should matchTo(expectedPP)
            }
          }
        }
      }
    }

    "addPending" - {
      // TODO DEX-1013 Test results
      "the new state should be equal to the previous with arguments' changes" in forAll(
        pessimisticPortfoliosGen,
        Gen.listOf(pessimisticTransactionGen)
      ) { (pp, arg) =>
        val before = getState(pp)
        pp.addPending(arg)
        val expected = combine(before, collectChanges(arg))
        filterAddressesBy(getState(pp), expected) should matchTo(expected)
      }
    }

    "processConfirmed" - {
      val testGen = for {
        initialTxs <- Gen.listOf(pessimisticTransactionGen)
        confirmedTxIds <- Gen.listOf(Gen.oneOf(mkPBTxId :: initialTxs.map(_.txId))) // + random tx id
      } yield (initialTxs, confirmedTxIds)

      // TODO DEX-1013 Test results
      "the new state combined with confirmed txs changes should be equal to the previous state" in forAll(testGen) {
        case (initialTxs, confirmedTxIds) =>
          val pp = mkPessimisticPortfolios(initialTxs)

          val initialTxsMap = initialTxs.map(x => x.txId -> x).toMap
          val confirmedTxsChanges =
            confirmedTxIds.distinct.foldMap(id => initialTxsMap.get(id).fold(Map.empty: AddressAssets)(_.pessimisticPortfolio))
          val before = getState(pp)

          pp.processConfirmed(confirmedTxIds)
          filterAddressesBy(combine(getState(pp), confirmedTxsChanges), before) should matchTo(before)
      }
    }

    "removeFailed" - {
      val testGen = for {
        initialTxs <- Gen.listOf(pessimisticTransactionGen)
        failedTxIds <- Gen.listOf(Gen.oneOf(mkPBTxId :: initialTxs.map(_.txId))) // + random tx id
      } yield (initialTxs, failedTxIds)

      // TODO DEX-1013 Test results
      "the new state combined with failed txs changes should be equal to the previous state" in forAll(testGen) {
        case (initialTxs, failedTxIds) =>
          val pp = mkPessimisticPortfolios(initialTxs)

          val initialTxsMap = initialTxs.map(x => x.txId -> x).toMap
          val confirmedTxsChanges = failedTxIds.distinct
            .foldMap(id => initialTxsMap.get(id).fold(Map.empty: AddressAssets)(_.pessimisticPortfolio))
          val before = getState(pp)

          pp.removeFailed(failedTxIds)

          filterAddressesBy(combine(getState(pp), confirmedTxsChanges), before) should matchTo(before)
      }
    }
  }

  def collectChanges(txs: List[PessimisticTransaction]): AddressAssets = combine(txs.map(_.pessimisticPortfolio): _*)

  def combine(xs: AddressAssets*): AddressAssets =
    Monoid.combineAll(xs).view.mapValues(_.filter(_._2 != 0)).filter(_._2.nonEmpty).toMap

  def getState(pp: PessimisticPortfolios): AddressAssets =
    addresses.view.map(x => x -> pp.getAggregated(x)).filter(_._2.nonEmpty).toMap

  def filterAddressesBy(xs: AddressAssets, by: AddressAssets): AddressAssets = xs.collect {
    case (address, xs) if by.contains(address) => address -> filterAssetsBy(xs, by(address))
  }

  def filterAssetsBy(xs: Map[Asset, Long], by: Map[Asset, Long]): Map[Asset, Long] = xs.filter { case (asset, _) => by.contains(asset) }

  def mkPessimisticPortfolios(initialTxs: List[PessimisticTransaction]): PessimisticPortfolios

}
