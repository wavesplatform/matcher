package com.wavesplatform.dex.grpc.integration.smart

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Blockchain, DataEntry, LeaseBalance, TxMeta, VolumeAndFee}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.ScriptRunnerFixed
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import shapeless.Coproduct

import scala.util.control.NoStackTrace

object MatcherScriptRunner {

  def apply(
    script: Script,
    order: Order,
    blockchain: Blockchain,
    isSynchronousCallsActivated: Boolean,
    useNewPowPrecision: Boolean
  ): Either[ExecutionError, EVALUATED] =
    ScriptRunnerFixed.applyGeneric(
      in = Coproduct[ScriptRunnerFixed.TxOrd](order),
      blockchain = blockchain,
      script = script,
      isAssetScript = false,
      scriptContainerAddress = Coproduct[Environment.Tthis](Recipient.Address(ByteStr(order.senderPublicKey.toAddress.bytes))),
      complexityLimit = Int.MaxValue,
      default = TRUE,
      isSynchronousCallsActivated,
      isSynchronousCallsActivated,
      useNewPowPrecision
    )._3

  private class Denied(methodName: String)
      extends SecurityException(s"An access to the blockchain.$methodName is denied on DEX")
      with NoStackTrace

  private def kill(methodName: String) = throw new Denied(methodName)

  val deniedBlockchain: Blockchain = new Blockchain {

    override def hasData(address: Address): Boolean = kill("hasData")

    override def transactionInfo(id: BlockId) = kill("transactionInfo")
    override def accountScript(address: Address) = kill("accountScript")
    override def blockHeader(height: Int) = kill("blockHeader")
    override def hitSource(height: Int) = kill("hitSource")
    override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]) = kill("balanceSnapshots")
    override def hasAccountScript(address: Address) = kill("hasAccountScript")
    override def resolveERC20Address(address: ERC20Address): Option[com.wavesplatform.transaction.Asset.IssuedAsset] = kill("resolveERC20Address")

    override def settings: BlockchainSettings = kill("settings")
    override def height: Int = kill("height")
    override def score: BigInt = kill("score")
    override def carryFee: Long = kill("carryFee")
    override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

    /** Features related */
    override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")
    override def activatedFeatures: Map[Short, Int] = kill("activatedFeatures")
    override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")
    override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")
    override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = kill("assetDescription")
    override def resolveAlias(a: Alias): Either[ValidationError, Address] = kill("resolveAlias")
    override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")
    override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

    /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
    override def accountData(acc: Address, key: String): Option[DataEntry[_]] = kill("accountData")
    override def leaseBalance(address: Address): LeaseBalance = kill("leaseBalance")
    override def balance(address: Address, mayBeAssetId: Asset): Long = kill("balance")

    override def transferById(id: BlockId): Option[(Int, TransferTransaction)] = kill("transferById")

    /** Block reward related */
    override def blockReward(height: Int): Option[Long] = kill("blockReward")
    override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")
    override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

    override def transactionMeta(id: BlockId): Option[TxMeta] = kill("transactionMeta")
    override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")
    override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = kill("assetScript")
  }

}
