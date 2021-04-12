package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import cats.syntax.option._
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.dex.domain.account.{Address, KeyPair}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.services.{TransactionDiff, UtxTransaction}
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.Amount
import org.scalacheck.{Arbitrary, Gen}

trait PBEntitiesGen {

  // Domain gen

  val accountGen: Gen[KeyPair] = Gen.containerOfN[Array, Byte](20, Arbitrary.arbByte.arbitrary).map(xs => KeyPair(ByteStr(xs)))
  val addressGen: Gen[Address] = accountGen.map(_.toAddress)

  val issuedAssetGen: Gen[Asset] = Gen.containerOfN[Array, Byte](32, Arbitrary.arbByte.arbitrary).map(IssuedAsset(_))

  // Const

  def mkPBTxId: ByteString = mkPBByteString32
  def mkPBAddress: ByteString = addressGen.map(_.toPB).sample.get
  def mkPBIssuedAsset: ByteString = mkPBByteString32
  def mkPBByteString32: ByteString = mkPBByteString(32)

  def mkPBByteString(len: Int): ByteString = mkPBByteStringGen(len).sample.get

  def mkUtxTransaction(
    id: ByteString,
    balanceUpdates: Seq[StateUpdate.BalanceUpdate] = Seq.empty,
    leaseUpdates: Seq[StateUpdate.LeasingUpdate] = Seq.empty
  ): UtxTransaction = UtxTransaction(
    id = id,
    diff = TransactionDiff(
      stateUpdate = StateUpdate(balanceUpdates, leaseUpdates).some
    ).some
  )

  // PB gen

  val pbByteString32Gen = mkPBByteStringGen(32)

  val pbAddressGen = addressGen.map(_.toPB)

  val pbWaves = ByteString.EMPTY
  val pbIssuedAssetGen = pbByteString32Gen
  val pbAssetGen = Gen.oneOf(pbIssuedAssetGen, Gen.const(pbWaves))

  val pbTxIdGen = pbByteString32Gen

  val pbEmptyBalanceUpdateGen: Gen[StateUpdate.BalanceUpdate] = pbAddressGen.map { address =>
    StateUpdate.BalanceUpdate(
      address = address
    )
  }

  def pbNonEmptyBalanceUpdateGen(amountGen: Gen[Long] = Arbitrary.arbLong.arbitrary): Gen[StateUpdate.BalanceUpdate] =
    for {
      address <- pbAddressGen
      asset <- pbAssetGen
      amount <- amountGen
    } yield StateUpdate.BalanceUpdate(
      address = address,
      amountAfter = Amount(asset, amount).some
    )

  val pbBalanceUpdateListGen = Gen
    .listOf(Gen.oneOf(pbEmptyBalanceUpdateGen, pbNonEmptyBalanceUpdateGen()))
    .suchThat(_.groupBy(_.address).forall(_._2.lengthCompare(1) == 0)) // Because an address affected only once in a transaction

  def pbLeasingUpdateGen(
    inGen: Gen[Long] = Arbitrary.arbLong.arbitrary,
    outGen: Gen[Long] = Arbitrary.arbLong.arbitrary
  ): Gen[StateUpdate.LeasingUpdate] =
    for {
      address <- pbAddressGen
      in <- inGen
      out <- outGen
    } yield StateUpdate.LeasingUpdate(
      address = address,
      inAfter = in,
      outAfter = out
    )

  val pbLeasingUpdateListGen = Gen
    .listOf(pbLeasingUpdateGen())
    .suchThat(_.groupBy(_.address).forall(_._2.lengthCompare(1) == 0)) // Because an address affected only once in a transaction

  val pbStateUpdateGen: Gen[StateUpdate] = for {
    balanceUpdates <- pbBalanceUpdateListGen
    leasingUpdates <- pbLeasingUpdateListGen
  } yield StateUpdate(balances = balanceUpdates, leasingForAddress = leasingUpdates)

  val pbUtxTransactionGen: Gen[UtxTransaction] =
    for {
      id <- pbTxIdGen
      update <- pbStateUpdateGen
    } yield UtxTransaction(
      id = id,
      diff = TransactionDiff(stateUpdate = update.some).some
    )

  def mkPBByteStringGen(len: Int): Gen[ByteString] =
    Gen.containerOfN[Array, Byte](len, Arbitrary.arbByte.arbitrary).map(UnsafeByteOperations.unsafeWrap)

}
