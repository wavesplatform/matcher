package com.wavesplatform.dex.grpc.integration.smart

import cats.implicits._
import cats.{Eval, Id}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{ExecutionError, Global, ValidationError}
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AssetDescription, Blockchain, DataEntry, LeaseBalance, Portfolio, VolumeAndFee}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.RealTransactionWrapper.ord
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.control.NoStackTrace

// Used only for order validation
object MatcherContext {

  def build(version: StdLibVersion, nByte: Byte, inE: Eval[Order]): Either[ExecutionError, EvaluationContext[Environment, Id]] = {
    val in: Coeval[Order] = Coeval.delay(inE.value)

    DirectiveSet(
      version,
      ScriptType.isAssetScript(false),
      ContentType.isDApp(false)
    ).map { ds =>
      val ctx =
        PureContext.build(Global, version).withEnvironment[Environment] |+|
          CryptoContext.build(Global, version).withEnvironment[Environment] |+|
          WavesContext.build(ds)

      ctx.evaluationContext(
        new WavesEnvironment(
          nByte = nByte,
          in = in.map(o => Coproduct[traits.Environment.InputEntity](ord(o))),
          h = Coeval.raiseError(new Denied("height")),
          blockchain = deniedBlockchain,
          address = in.map(x => ByteStr(x.senderPublicKey.toAddress.bytes)),
          ds = ds,
          txId = ByteStr.empty // TODO throw ?
        ))
    }
  }

  private class Denied(methodName: String) extends SecurityException(s"An access to the blockchain.$methodName is denied on DEX") with NoStackTrace

  private def kill(methodName: String) = throw new Denied(methodName)

  private val deniedBlockchain = new Blockchain {

    override def transactionInfo(id: BlockId)                                       = kill("transactionInfo")
    override def accountScript(address: Address)                                    = kill("accountScript")
    override def assetScript(id: Asset.IssuedAsset): Option[(Script, Long)]         = kill("assetScript")
    override def blockHeader(height: Int)                                           = kill("blockHeader")
    override def hitSource(height: Int)                                             = kill("hitSource")
    override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]) = kill("balanceSnapshots")
    override def hasAccountScript(address: Address)                                 = kill("hasAccountScript")
    override def collectActiveLeases(filter: LeaseTransaction => Boolean)           = kill("collectActiveLeases")

    override def settings: BlockchainSettings            = kill("settings")
    override def height: Int                             = kill("height")
    override def score: BigInt                           = kill("score")
    override def carryFee: Long                          = kill("carryFee")
    override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

    /** Features related */
    override def approvedFeatures: Map[Short, Int]                                 = kill("approvedFeatures")
    override def activatedFeatures: Map[Short, Int]                                = kill("activatedFeatures")
    override def featureVotes(height: Int): Map[Short, Int]                        = kill("featureVotes")
    override def transactionHeight(id: ByteStr): Option[Int]                       = kill("transactionHeight")
    override def containsTransaction(tx: Transaction): Boolean                     = kill("containsTransaction")
    override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = kill("assetDescription")
    override def resolveAlias(a: Alias): Either[ValidationError, Address]          = kill("resolveAlias")
    override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]              = kill("leaseDetails")
    override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee                = kill("filledVolumeAndFee")

    /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
    override def accountData(acc: Address, key: String): Option[DataEntry[_]] = kill("accountData")
    override def leaseBalance(address: Address): LeaseBalance                 = kill("leaseBalance")
    override def balance(address: Address, mayBeAssetId: Asset): Long         = kill("balance")

    /**
      * Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
      * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance
      */
    override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = kill("collectLposPortfolios")
    override def transferById(id: BlockId): Option[(Int, TransferTransaction)]                           = kill("transferById")

    /** Block reward related */
    override def blockReward(height: Int): Option[Long]   = kill("blockReward")
    override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")
    override def wavesAmount(height: Int): BigInt         = kill("wavesAmount")

    override def balanceOnlySnapshots(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceOnlySnapshots")
  }
}
