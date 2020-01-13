package com.wavesplatform.dex.it.config.genesis

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Signed
import com.wavesplatform.dex.domain.error.ValidationError.GenericError
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.it.config.genesis.SignerData
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.reflect.ClassTag
import scala.util.Try

case class Block(timestamp: Long,
                 version: Byte,
                 reference: ByteStr,
                 signerData: SignerData,
                 consensusData: NxtLikeConsensusBlockData,
                 transactionData: Seq[GenesisTransaction],
                 featureVotes: Set[Short],
                 rewardVote: Long)
    extends Signed {

  import Block._

  val sender: PublicKey = signerData.generator
  val maxLength: Int    = 150 * 1024

//  private val transactionField = TransactionsBlockField(version.toInt, transactionData)
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

  val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(bytes().dropRight(crypto.SignatureLength))

//  val blockScore: Coeval[BigInt] = Coeval.evalOnce((BigInt("18446744073709551616") / consensusData.baseTarget).ensuring(_ > 0))

//  val feesPortfolio: Coeval[Portfolio] = Coeval.evalOnce(Monoid[Portfolio].combineAll({
//    val assetFees: Seq[(Asset, Long)] = transactionData.map(_.assetFee)
//    assetFees
//      .map { case (maybeAssetId, vol) => maybeAssetId -> vol }
//      .groupBy(a => a._1)
//      .mapValues((records: Seq[(Asset, Long)]) => records.map(_._2).sum)
//  }.toList.map {
//    case (assetId, feeVolume) =>
//      assetId match {
//        case Waves                  => Portfolio(feeVolume, LeaseBalance.empty, Map.empty)
//        case asset @ IssuedAsset(_) => Portfolio(0L, LeaseBalance.empty, Map(asset -> feeVolume))
//      }
//  }))
//
//  val prevBlockFeePart: Coeval[Portfolio] =
//    Coeval.evalOnce(Monoid[Portfolio].combineAll(transactionData.map(tx => tx.feeDiff().minus(tx.feeDiff().multiply(CurrentBlockFeePart)))))

  override val signatureValid: Coeval[Boolean] = Coeval.evalOnce {
    val publicKey = signerData.generator
    !crypto.isWeakPublicKey(publicKey) && crypto.verify(signerData.signature.arr, bytesWithoutSignature(), publicKey)
  }

  implicit class Cast[A](a: A) {
    def cast[B: ClassTag]: Option[B] = a match {
      case b: B => Some(b)
      case _    => None
    }
  }

  protected override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(transactionData.flatMap(_.cast[Signed]))

  override def toString: String =
    s"Block(${signerData.signature} -> ${reference.trim}, " +
      s"txs=${transactionData.size}, features=$featureVotes${if (rewardVote >= 0) s", rewardVote=$rewardVote" else ""})"

//  def getHeader(): BlockHeader =
//    new BlockHeader(timestamp, version, reference, signerData, consensusData, transactionData.length, featureVotes, rewardVote)
}

object Block /* extends ScorexLogging */ {

//  case class Fraction(dividend: Int, divider: Int) {
//    def apply(l: Long): Long = l / divider * dividend
//  }
//
//  val CurrentBlockFeePart: Fraction = Fraction(2, 5)

//  type BlockIds = Seq[ByteStr]
//  type BlockId  = ByteStr
//  val MaxTransactionsPerBlockVer1Ver2: Int = 100
//  val MaxTransactionsPerBlockVer3: Int     = 6000
  val MaxFeaturesInBlock: Int = 64
//  val BaseTargetLength: Int                = 8
  val GeneratorSignatureLength: Int = 32

//  val BlockIdLength: Int = SignatureLength

//  val TransactionSizeLength = 4

//  def transParseBytes(version: Int, bytes: Array[Byte]): Try[Seq[Transaction]] = Try {
//    if (bytes.isEmpty) {
//      Seq.empty
//    } else {
//      val v: (Array[Byte], Int) = version match {
//        case Block.GenesisBlockVersion | Block.PlainBlockVersion => (bytes.tail, bytes.head) //  127 max, won't work properly if greater
//        case Block.NgBlockVersion | Block.RewardBlockVersion =>
//          val size = ByteBuffer.wrap(bytes, 0, 4).getInt()
//          (bytes.drop(4), size)
//        case _ => ???
//      }
//
//      val txs = Seq.newBuilder[Transaction]
//      (1 to v._2).foldLeft(0) {
//        case (pos, _) =>
//          val transactionLengthBytes = v._1.slice(pos, pos + TransactionSizeLength)
//          val transactionLength      = Ints.fromByteArray(transactionLengthBytes)
//          val transactionBytes       = v._1.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
//          txs += TransactionParsers.parseBytes(transactionBytes).get
//          pos + TransactionSizeLength + transactionLength
//      }
//
//      txs.result()
//    }
//  }

//  def parseBytes(bytes: Array[Byte]): Try[Block] =
//    for {
//      (blockHeader, transactionBytes) <- BlockHeader.parseBytes(bytes)
//      transactionsData                <- transParseBytes(blockHeader.version, transactionBytes)
//      block <- build(
//        blockHeader.version,
//        blockHeader.timestamp,
//        blockHeader.reference,
//        blockHeader.consensusData,
//        transactionsData,
//        blockHeader.signerData,
//        blockHeader.featureVotes,
//        blockHeader.rewardVote
//      ).left.map(ve => new IllegalArgumentException(ve.toString)).toTry
//    } yield block
//
//  def areTxsFitInBlock(blockVersion: Byte, txsCount: Int): Boolean = {
//    (blockVersion == 3 && txsCount <= MaxTransactionsPerBlockVer3) || (blockVersion <= 2 || txsCount <= MaxTransactionsPerBlockVer1Ver2)
//  }
//
//  def fromHeaderAndTransactions(h: BlockHeader, txs: Seq[Transaction]): Either[GenericError, Block] = {
//    build(
//      h.version,
//      h.timestamp,
//      h.reference,
//      h.consensusData,
//      txs,
//      h.signerData,
//      h.featureVotes,
//      h.rewardVote
//    )
//  }

  def build(version: Byte,
            timestamp: Long,
            reference: ByteStr,
            consensusData: NxtLikeConsensusBlockData,
            transactionData: Seq[GenesisTransaction],
            signerData: SignerData,
            featureVotes: Set[Short],
            rewardVote: Long): Either[GenericError, Block] = {
    (for {
      _ <- Either.cond(reference.arr.length == crypto.SignatureLength, (), "Incorrect reference")
      _ <- Either.cond(consensusData.generationSignature.arr.length == GeneratorSignatureLength, (), "Incorrect consensusData.generationSignature")
      _ <- Either.cond(signerData.generator.length == crypto.KeyLength, (), "Incorrect signer")
      _ <- Either.cond(version > 2 || featureVotes.isEmpty, (), s"Block version $version could not contain feature votes")
      _ <- Either.cond(featureVotes.size <= MaxFeaturesInBlock, (), s"Block could not contain more than $MaxFeaturesInBlock feature votes")
    } yield Block(timestamp, version, reference, signerData, consensusData, transactionData, featureVotes, rewardVote)).left.map(GenericError(_))
  }

  def buildAndSign(version: Byte,
                   timestamp: Long,
                   reference: ByteStr,
                   consensusData: NxtLikeConsensusBlockData,
                   transactionData: Seq[GenesisTransaction],
                   signer: KeyPair,
                   featureVotes: Set[Short],
                   rewardVote: Long): Either[GenericError, Block] =
    build(version, timestamp, reference, consensusData, transactionData, SignerData(signer, ByteStr.empty), featureVotes, rewardVote).right
      .map(unsigned => unsigned.copy(signerData = SignerData(signer, ByteStr(crypto.sign(signer, unsigned.bytes())))))

//  def genesisTransactions(gs: GenesisSettings): Seq[GenesisTransaction] = {
//    gs.transactions.map { ts =>
//      val acc = Address.fromString(ts.recipient).explicitGet()
//      GenesisTransaction.create(acc, ts.amount, gs.timestamp).explicitGet()
//    }
//  }
//
//  def genesis(genesisSettings: GenesisSettings): Either[ValidationError, Block] = {
//    val genesisSigner = KeyPair(ByteStr.empty)
//
//    val transactionGenesisData      = genesisTransactions(genesisSettings)
//    val transactionGenesisDataField = TransactionsBlockFieldVersion1or2(transactionGenesisData)
//    val consensusGenesisData        = NxtLikeConsensusBlockData(genesisSettings.initialBaseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte)))
//    val consensusGenesisDataField   = NxtConsensusBlockField(consensusGenesisData)
//    val txBytesSize                 = transactionGenesisDataField.bytes().length
//    val txBytes                     = Bytes.ensureCapacity(Ints.toByteArray(txBytesSize), 4, 0) ++ transactionGenesisDataField.bytes()
//    val cBytesSize                  = consensusGenesisDataField.bytes().length
//    val cBytes                      = Bytes.ensureCapacity(Ints.toByteArray(cBytesSize), 4, 0) ++ consensusGenesisDataField.bytes()
//
//    val reference = Array.fill(SignatureLength)(-1: Byte)
//
//    val timestamp = genesisSettings.blockTimestamp
//    val toSign: Array[Byte] = Array(GenesisBlockVersion) ++
//      Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0) ++
//      reference ++
//      cBytes ++
//      txBytes ++
//      genesisSigner.publicKey.arr
//
//    val signature = genesisSettings.signature.fold(crypto.sign(genesisSigner, toSign))(_.arr)
//
//    for {
//      // Verify signature
//      _ <- Either.cond(crypto.verify(signature, toSign, genesisSigner.publicKey), (), GenericError("Passed genesis signature is not valid"))
//
//      // Verify initial balance
//      genesisTransactionsSum = transactionGenesisData.map(_.amount).reduce(Math.addExact(_: Long, _: Long))
//      _ <- Either.cond(
//        genesisTransactionsSum == genesisSettings.initialBalance,
//        (),
//        GenericError(s"Initial balance ${genesisSettings.initialBalance} did not match the distributions sum $genesisTransactionsSum")
//      )
//    } yield
//      Block(
//        timestamp = timestamp,
//        version = GenesisBlockVersion,
//        reference = ByteStr(reference),
//        signerData = SignerData(genesisSigner, ByteStr(signature)),
//        consensusData = consensusGenesisData,
//        transactionData = transactionGenesisData,
//        featureVotes = Set.empty,
//        rewardVote = -1L
//      )
//  }

//  val GenesisBlockVersion: Byte = 1
//  val PlainBlockVersion: Byte   = 2
//  val NgBlockVersion: Byte      = 3
//  val RewardBlockVersion: Byte  = 4
}
