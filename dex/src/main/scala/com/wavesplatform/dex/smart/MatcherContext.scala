package com.wavesplatform.dex.smart

import cats.{Eval, Id}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, BalanceSnapshot, Blockchain, DataEntry, InvokeScriptResult, LeaseBalance, Portfolio, TransactionId, VolumeAndFee}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.BlockchainContext
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.control.NoStackTrace

// Used only for order validation
object MatcherContext {

  def build(version: StdLibVersion, nByte: Byte, inE: Eval[Order], isDApp: Boolean): Either[ExecutionError, EvaluationContext[Environment, Id]] = {
    val in: Coeval[Order] = Coeval.delay(inE.value)
    BlockchainContext
      .build(
        version,
        nByte,
        in.map(o => Coproduct[BlockchainContext.In](o)),
        Coeval.raiseError(new Denied("height")),
        deniedBlockchain,
        isTokenContext = false,
        isContract = isDApp,
        in.map(_.senderPublicKey.toAddress.bytes)
      )
  }

  private class Denied(methodName: String) extends SecurityException(s"An access to the blockchain.$methodName is denied on DEX") with NoStackTrace
  private def kill(methodName: String) = throw new Denied(methodName)

  private val deniedBlockchain = new Blockchain {
    override def settings: BlockchainSettings                                     = kill("settings")
    override def height: Int                                                      = kill("height")
    override def score: BigInt                                                    = kill("score")
    override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)]      = kill("blockHeaderAndSize")
    override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = kill("blockHeaderAndSize")
    override def lastBlock: Option[Block]                                         = kill("lastBlock")
    override def carryFee: Long                                                   = kill("carryFee")
    override def blockBytes(height: Int): Option[Array[Byte]]                     = kill("blockBytes")
    override def blockBytes(blockId: ByteStr): Option[Array[Byte]]                = kill("blockBytes")
    override def heightOf(blockId: ByteStr): Option[Int]                          = kill("heightOf")

    /** Returns the most recent block IDs, starting from the most recent  one */
    override def lastBlockIds(howMany: Int): Seq[ByteStr] = kill("lastBlockIds")

    /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
    override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = kill("blockIdsAfter")
    override def parentHeader(block: BlockHeader, back: Int): Option[BlockHeader]            = kill("parentHeader")
    override def totalFee(height: Int): Option[Long]                                         = kill("totalFee")

    /** Features related */
    override def approvedFeatures: Map[Short, Int]                                 = kill("approvedFeatures")
    override def activatedFeatures: Map[Short, Int]                                = kill("activatedFeatures")
    override def featureVotes(height: Int): Map[Short, Int]                        = kill("featureVotes")
    override def transactionInfo(id: ByteStr): Option[(Int, Transaction)]          = kill("transactionInfo")
    override def transactionHeight(id: ByteStr): Option[Int]                       = kill("transactionHeight")
    override def containsTransaction(tx: Transaction): Boolean                     = kill("containsTransaction")
    override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = kill("assetDescription")
    override def resolveAlias(a: Alias): Either[ValidationError, Address]          = kill("resolveAlias")
    override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]              = kill("leaseDetails")
    override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee                = kill("filledVolumeAndFee")

    /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
    override def balanceSnapshots(address: Address, from: Int, to: BlockId): Seq[BalanceSnapshot] = kill("balanceSnapshots")
    override def accountScript(address: Address): Option[Script]                                  = kill("accountScript")
    override def hasScript(address: Address): Boolean                                             = kill("hasScript")
    override def assetScript(id: Asset.IssuedAsset): Option[Script]                               = kill("assetScript")
    override def hasAssetScript(id: Asset.IssuedAsset): Boolean                                   = kill("hasAssetScript")
    override def accountDataKeys(address: Address): Set[String]                                   = kill("accountDataKeys")
    override def accountData(acc: Address, key: String): Option[DataEntry[_]]                     = kill("accountData")
    override def accountData(acc: Address): AccountDataInfo                                       = kill("accountData")
    override def leaseBalance(address: Address): LeaseBalance                                     = kill("leaseBalance")
    override def balance(address: Address, mayBeAssetId: Asset): Long                             = kill("balance")

    /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
      *
      * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
    override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = kill("collectLposPortfolios")
    override def invokeScriptResult(txId: TransactionId): Either[ValidationError, InvokeScriptResult]    = kill("invokeScriptResult")
    override def transferById(id: BlockId): Option[(Int, TransferTransaction)]                           = kill("transferById")

    /** Block reward related */
    override def blockReward(height: Int): Option[Long]   = kill("blockReward")
    override def lastBlockReward: Option[Long]            = kill("lastBlockReward")
    override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

    override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

    override def accountScriptWithComplexity(address: Address): Option[(Script, Long)]                               = kill("accountScriptWithComplexity")
    override def assetScriptWithComplexity(id: Asset.IssuedAsset): Option[(Script, Long)]                            = kill("assetScriptWithComplexity")
    override def collectActiveLeases(from: Int, to: Int)(filter: LeaseTransaction => Boolean): Seq[LeaseTransaction] = kill("collectActiveLeases")

    override def balanceOnlySnapshots(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceOnlySnapshots")
  }

}
