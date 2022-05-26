package com.wavesplatform.it.sync.metamask

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.web3j.abi.FunctionEncoder
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto.{ECKeyPair, RawTransaction, Sign, SignedRawTransaction, TransactionEncoder}
import org.web3j.utils._
import org.web3j.crypto._

object EthTransactionsHelper {

  type TxId = ByteStr
  type TxData = ByteStr

  def generateEthTransfer(
    chainId: Long,
    keyPair: ECKeyPair,
    recipient: Address,
    amount: Long,
    asset: Asset,
    fee: Long
  ): (TxId, TxData) = {

    def signRawTransaction(rawTransaction: RawTransaction): (TxId, TxData) = {
      val signed = new SignedRawTransaction(
        rawTransaction.getTransaction,
        TransactionEncoder.createEip155SignatureData(
          Sign.signMessage(TransactionEncoder.encode(rawTransaction, chainId.toLong), keyPair, true),
          chainId.toLong
        )
      )
      val txData = encodeMethod.invoke(null, rawTransaction, signed.getSignatureData).asInstanceOf[Array[Byte]]
      (ByteStr(Hash.sha3(txData)), ByteStr(txData))
    }

    val ethRecipient = Numeric.toHexString(recipient.bytes.drop(2).dropRight(4))

    asset match {
      case Asset.Waves =>
        signRawTransaction {
          RawTransaction.createTransaction(
            BigInt(System.currentTimeMillis()).bigInteger,
            DefaultGasPrice,
            BigInt(fee).bigInteger,
            ethRecipient,
            (BigInt(amount) * AmountMultiplier).bigInteger,
            ""
          )
        }

      case Asset.IssuedAsset(assetId) =>
        import scala.jdk.CollectionConverters._
        import org.web3j.abi.{datatypes => ethTypes}
        signRawTransaction {
          val function = new org.web3j.abi.datatypes.Function(
            "transfer",
            Seq[ethTypes.Type[_]](
              new ethTypes.Address(ethRecipient),
              new ethTypes.generated.Uint256(amount)
            ).asJava,
            Nil.asJava
          )

          RawTransaction.createTransaction(
            BigInt(System.currentTimeMillis()).bigInteger,
            DefaultGasPrice,
            BigInt(fee).bigInteger,
            Numeric.toHexString(assetId.arr.take(20)),
            FunctionEncoder.encode(function)
          )
        }
    }
  }

  private val AmountMultiplier = 10000000000L

  private val DefaultGasPrice = Convert.toWei("10", Convert.Unit.GWEI).toBigInteger

  private val encodeMethod = {
    val m = classOf[TransactionEncoder].getDeclaredMethod("encode", classOf[RawTransaction], classOf[SignatureData])
    m.setAccessible(true)
    m
  }

}
