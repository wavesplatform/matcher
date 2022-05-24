package com.wavesplatform.it.sync.metamask

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
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
    val ethRecipient = Numeric.toHexString(recipient.bytes.drop(2).dropRight(4))
    asset match {
      case Asset.Waves =>
        val raw =
          RawTransaction.createTransaction(
            BigInt(System.currentTimeMillis()).bigInteger,
            DefaultGasPrice,
            BigInt(fee).bigInteger,
            ethRecipient,
            (BigInt(amount) * AmountMultiplier).bigInteger,
            ""
          )
        val signed =
          new SignedRawTransaction(
            raw.getTransaction,
            TransactionEncoder.createEip155SignatureData(
              Sign.signMessage(TransactionEncoder.encode(raw, chainId.toLong), keyPair, true),
              chainId.toLong
            )
          )

        val txData = encodeMethod.invoke(null, raw, signed.getSignatureData).asInstanceOf[Array[Byte]]
        (ByteStr(Hash.sha3(txData)), ByteStr(txData))

      case Asset.IssuedAsset(_) => ???
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
