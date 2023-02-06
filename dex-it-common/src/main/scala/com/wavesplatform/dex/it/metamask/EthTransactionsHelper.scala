package com.wavesplatform.dex.it.metamask

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.web3j.abi.datatypes.{AbiTypes, StructType}
import org.web3j.abi.{FunctionEncoder, datatypes => ethTypes}
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto._
import org.web3j.utils._

import scala.jdk.CollectionConverters._

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
    val ethRecipient = mkEthAddress(recipient)

    asset match {
      case Asset.Waves =>
        signRawTransaction(chainId, keyPair) {
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
        signRawTransaction(chainId, keyPair) {
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

  def generateEthInvoke(
    chainId: Long,
    keyPair: ECKeyPair,
    address: Address,
    funcName: String,
    args: Seq[Arg],
    payments: Seq[Payment],
    fee: Long
  ): (TxId, TxData) = {
    val ethAddress = mkEthAddress(address)

    val paymentsArg = {
      val tuples = payments.toVector.map { p =>
        val assetId = p.assetId match {
          case Asset.IssuedAsset(id) => id
          case Asset.Waves => WavesByteRepr
        }
        Arg.Struct(Arg.Bytes(assetId, "bytes32"), Arg.Integer(p.amount))
      }
      Arg.List(Arg.Struct(Arg.Bytes(WavesByteRepr, "bytes32"), Arg.Integer(0)), tuples)
    }
    val fullArgs = args :+ paymentsArg
    val argsAsEth = fullArgs.map(toEthType)
    val function = new org.web3j.abi.datatypes.Function(
      funcName,
      argsAsEth.asJava,
      Nil.asJava
    )

    signRawTransaction(chainId, keyPair) {
      RawTransaction.createTransaction(
        BigInt(System.currentTimeMillis()).bigInteger,
        DefaultGasPrice,
        BigInt(fee).bigInteger,
        ethAddress,
        FunctionEncoder.encode(function)
      )
    }
  }

  private def signRawTransaction(chainId: Long, keyPair: ECKeyPair)(rawTransaction: RawTransaction): (TxId, TxData) = {
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

  private def mkEthAddress(address: Address): String =
    Numeric.toHexString(address.bytes.drop(2).dropRight(4))

  private def toEthType(value: Arg): ethTypes.Type[_] = value match {
    case Arg.Integer(v, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[Long]).newInstance(v)
    case Arg.BigInteger(bi, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[java.math.BigInteger]).newInstance(bi.bigInteger)
    case Arg.Str(v) =>
      new ethTypes.Utf8String(v)
    case Arg.Bytes(v, typeStr) =>
      val typeClass = ethTypes.AbiTypes.getType(typeStr)
      typeClass.getConstructor(classOf[Array[Byte]]).newInstance(v.arr)
    case Arg.Bool(b) =>
      new ethTypes.Bool(b)
    case Arg.List(listType, elements) =>
      val ethTypedXs = elements.map(toEthType)
      val arrayClass = toEthType(listType)
      new ethTypes.DynamicArray(arrayClass.getClass.asInstanceOf[Class[ethTypes.Type[_]]], ethTypedXs: _*) {
        override def getTypeAsString: String =
          (if (classOf[StructType].isAssignableFrom(arrayClass.getClass)) arrayClass.getTypeAsString
           else AbiTypes.getTypeAString(getComponentType)) + "[]"
      }
    case x: Arg.Struct => new ethTypes.StaticStruct(x.values.map(toEthType): _*)
  }

  private val WavesByteRepr = ByteStr(new Array[Byte](32))

  private val AmountMultiplier = 10000000000L

  private val DefaultGasPrice = Convert.toWei("10", Convert.Unit.GWEI).toBigInteger

  private val encodeMethod = {
    val m = classOf[TransactionEncoder].getDeclaredMethod("encode", classOf[RawTransaction], classOf[SignatureData])
    m.setAccessible(true)
    m
  }

  sealed trait Arg

  object Arg {
    final case class Integer(v: Long, typeStr: String = "int64") extends Arg
    final case class Bytes(v: ByteStr, typeStr: String = "bytes") extends Arg
    final case class Str(v: String) extends Arg
    final case class BigInteger(bi: BigInt, typeStr: String = "int256") extends Arg
    final case class Bool(b: Boolean) extends Arg
    final case class List(listType: Arg, elements: Seq[Arg]) extends Arg
    final case class Struct(values: Arg*) extends Arg
  }

  final case class Payment(amount: Long, assetId: Asset)

}
