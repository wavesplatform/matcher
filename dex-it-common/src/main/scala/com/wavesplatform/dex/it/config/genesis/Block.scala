package com.wavesplatform.dex.it.config.genesis

import java.io.ByteArrayOutputStream

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Authorized
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import monix.eval.Coeval

/** Simplified version of the Node's block. The only purpose is to supply correct values for the waves.blockchain.custom.genesis settings */
case class Block(timestamp: Long,
                 version: Byte,
                 reference: ByteStr,
                 signerData: SignerData,
                 consensusData: NxtLikeConsensusBlockData,
                 transactionData: Seq[GenesisTransaction])
    extends Authorized {

  override val sender: PublicKey = signerData.generator
  private val maxLength: Int     = 150 * 1024

  private val transactionField: Array[Byte] = {

    val serTxCount = Array(transactionData.size.toByte)
    val byteBuffer = new ByteArrayOutputStream(transactionData.size * maxLength / 2)

    byteBuffer.write(serTxCount, 0, serTxCount.length)

    transactionData.foreach { tx =>
      val txBytes = tx.bytes()
      val txSize  = Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0)

      byteBuffer.write(txSize, 0, txSize.length)
      byteBuffer.write(txBytes, 0, txBytes.length)
    }

    byteBuffer.toByteArray
  }

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {

    val txBytesSize = transactionField.length
    val txBytes     = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionField

    val consensusField = Bytes.ensureCapacity(Longs.toByteArray(consensusData.baseTarget), 8, 0) ++ consensusData.generationSignature.arr

    val cBytesSize = consensusField.length
    val cBytes     = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusField

    Array(version) ++
      Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0) ++
      reference.arr ++
      cBytes ++
      txBytes ++
      signerData.generator.arr ++
      signerData.signature.arr
  }
}

object Block {

  val MaxFeaturesInBlock: Int       = 64
  val GeneratorSignatureLength: Int = 32

  def build(version: Byte,
            timestamp: Long,
            reference: ByteStr,
            consensusData: NxtLikeConsensusBlockData,
            transactionData: Seq[GenesisTransaction],
            signerData: SignerData): Either[GenericError, Block] = {
    (for {
      _ <- Either.cond(reference.arr.length == crypto.SignatureLength, (), "Incorrect reference")
      _ <- Either.cond(consensusData.generationSignature.arr.length == GeneratorSignatureLength, (), "Incorrect consensusData.generationSignature")
      _ <- Either.cond(signerData.generator.length == crypto.KeyLength, (), "Incorrect signer")
    } yield Block(timestamp, version, reference, signerData, consensusData, transactionData)).left.map(GenericError(_))
  }

  def buildAndSign(version: Byte,
                   timestamp: Long,
                   reference: ByteStr,
                   consensusData: NxtLikeConsensusBlockData,
                   transactionData: Seq[GenesisTransaction],
                   signer: KeyPair): Either[GenericError, Block] =
    build(version, timestamp, reference, consensusData, transactionData, SignerData(signer, ByteStr.empty))
      .map(unsigned => unsigned.copy(signerData = SignerData(signer, ByteStr(crypto.sign(signer, unsigned.bytes.value)))))
}
