package com.wavesplatform.dex

import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.softwaremill.diffx.{ConsoleColorConfig, Derived, Diff, DiffResultDifferent, DiffResultValue, FieldPath, Identical}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.clients.domain.{TransactionWithChanges, WavesBlock, WavesChain}
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.SignedTransaction
import io.qameta.allure.scalatest.AllureScalatestContext
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.enablers.Emptiness
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.collection.immutable.Vector
import scala.concurrent.duration._

// TODO DEX-994
trait WavesIntegrationSuiteBase extends AnyFreeSpecLike with Matchers with AllureScalatestContext with ScalaFutures {

  implicit override def patienceConfig = PatienceConfig(5.seconds)

  // scalatest

  implicit val optionEmptiness: Emptiness[Option[Any]] = (thing: Option[Any]) => thing.isEmpty

  // diffx
  val byteStringDiff: Diff[ByteString] = Diff[String].contramap[ByteString](xs => Base58.encode(xs.toByteArray))

  implicit val derivedByteStringDiff: Derived[Diff[ByteString]] = Derived(byteStringDiff)
  implicit val derivedUtxTransactionDiff: Derived[Diff[UtxTransaction]] = Derived(byteStringDiff.contramap[UtxTransaction](_.id))

  implicit val addressDiff: Diff[Address] = Diff[String].contramap[Address](_.stringRepr)

  // TODO Duplicate
  implicit val issuedAssetDiff: Diff[IssuedAsset] = { (left: IssuedAsset, right: IssuedAsset, _: List[FieldPath]) =>
    if (left.id == right.id) Identical(left) else DiffResultValue(left, right)
  }

  // TODO Duplicate
  implicit val assetDiff: Diff[Asset] = { (left: Asset, right: Asset, _: List[FieldPath]) =>
    if (left == right) Identical(left) else DiffResultValue(left, right)
  }

  // TODO Duplicate
  implicit val issuedAssetDerivedDiff: Derived[Diff[IssuedAsset]] = Derived(issuedAssetDiff)

  // TODO Duplicate
  implicit val assetDerivedDiff: Derived[Diff[Asset]] = Derived(assetDiff)

  // Fixes "Class too large" compiler issue
  implicit val derivedSignedTransactionDiff: Derived[Diff[TransactionWithChanges]] =
    Derived(byteStringDiff.contramap[TransactionWithChanges](_.txId))

  def getDiff[T](comparison: (T, T) => Boolean): Diff[T] = { (left: T, right: T, _: List[FieldPath]) =>
    if (comparison(left, right)) Identical(left) else DiffResultValue(left, right)
  }

  def matchTo[A: Diff](right: A)(implicit c: ConsoleColorConfig): Matcher[A] = { left =>
    Diff[A].apply(left, right) match {
      case c: DiffResultDifferent =>
        val diff = c.show.split('\n').mkString(Console.RESET, s"${Console.RESET}\n${Console.RESET}", Console.RESET)
        MatchResult(matches = false, s"Matching error:\n$diff\nleft: $left", "")
      case _ => MatchResult(matches = true, "", "")
    }
  }

  protected def mkTransactionWithChangesMap(id: Int): Map[ByteString, TransactionWithChanges] = {
    val txId = mkTxId(id)
    Map(txId -> mkTransactionWithChanges(txId))
  }

  protected def mkUtxTransactionMap(id: Int): Map[ByteString, UtxTransaction] = {
    val txId = mkTxId(id)
    Map(txId -> UtxTransaction(txId))
  }

  protected def mkTransactionWithChanges(txId: ByteString): TransactionWithChanges = TransactionWithChanges(
    txId = txId,
    tx = SignedTransaction(),
    changes = StateUpdate()
  )

  protected def mkTxId(n: Int): ByteString = {
    require(n <= 127) // or we need complex implementation
    UnsafeByteOperations.unsafeWrap(new Array[Byte](n))
  }

  /**
   * If history is empty, the height is supposed to be 0
   */
  protected def mkChain(history: Vector[WavesBlock], blocksCapacity: Int): WavesChain =
    WavesChain(history, history.headOption.getOrElse(throw new IllegalArgumentException("history is empty")).ref.height, blocksCapacity)

}
