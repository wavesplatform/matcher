package com.wavesplatform.dex.meta

import com.wavesplatform.dex.domain.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.feature.BlockchainFeature
import com.wavesplatform.dex.domain.order.{Order, OrderV3}
import shapeless._

import scala.annotation.nowarn
import scala.reflect.ClassTag

trait Sample[T] {
  def sample: T
}

object Sample {
  def mk[T](x: => T): Sample[T] = new Sample[T] {
    override def sample: T = x
  }

  implicit def assetMap[V](implicit v: Sample[V]): Sample[Map[Asset, V]] =
    mk(
      Map(
        Sample[Asset]       -> v.sample,
        Sample[IssuedAsset] -> v.sample
      ))

  implicit def array[T](implicit s: Sample[T], ct: ClassTag[T]): Sample[Array[T]] = mk(Array(s.sample))
  implicit def list[T](implicit s: Sample[T]): Sample[List[T]]                    = mk(List(s.sample))
  implicit def set[T](implicit s: Sample[T]): Sample[Set[T]]                      = mk(Set(s.sample))
  implicit def map[K, V](implicit m: Sample[K], v: Sample[V]): Sample[Map[K, V]]  = mk(Map(m.sample -> v.sample))

  def apply[A](implicit r: Sample[A]): A = r.sample

  implicit def generic[A, R](implicit gen: Generic.Aux[A, R], sample: Lazy[Sample[R]]): Sample[A] = mk(gen.from(sample.value.sample))

  implicit val hNil: Sample[HNil] = mk(HNil)
  implicit def hList[H, T <: HList](implicit hSample: Lazy[Sample[H]], tSample: Sample[T]): Sample[H :: T] =
    mk(hSample.value.sample :: tSample.sample)

  implicit val cNil: Sample[CNil] = mk(throw new Exception("Impossibru!"))
  implicit def coProduct[H, T <: Coproduct, L <: Nat](
      implicit
      hSample: Lazy[Sample[H]],
      tSample: Sample[T]
  ): Sample[H :+: T] = mk(Inl(hSample.value.sample))

  implicit val boolean: Sample[Boolean]         = mk(false)
  implicit val byte: Sample[Byte]               = mk(1)
  implicit val short: Sample[Short]             = mk(2)
  implicit val int: Sample[Int]                 = mk(4)
  implicit val long: Sample[Long]               = mk(5)
  implicit val double: Sample[Double]           = mk(6.1)
  implicit val decimal: Sample[BigDecimal]      = mk(BigDecimal(7.2))
  implicit val string: Sample[String]           = mk("some string")
  implicit val asset: Sample[Asset]             = mk(IssuedAsset(ByteStr.decodeBase58("Co11Tbj83TeZCnuXrZQwv6Bs4XG2sm1vGQiQ4FLDjJWs").get))
  implicit val byteStr: Sample[ByteStr]         = mk("byteStr".getBytes)
  implicit val issuedAsset: Sample[IssuedAsset] = mk(IssuedAsset(ByteStr.decodeBase58("Emn8cyGDFgnLCKLTXqVWhKJARhtR2muBUuZaSmqNzDfn").get))
  implicit val assetPair: Sample[AssetPair]     = mk(AssetPair(issuedAsset.sample, asset.sample))
  implicit val address: Sample[Address]         = mk(KeyPair(ByteStr("address".getBytes)).toAddress)
  implicit val publicKey: Sample[PublicKey]     = mk(PublicKey(Sample[ByteStr]))

  @nowarn implicit val order: Sample[Order] = mk(Sample[OrderV3])

  implicit val blockchainFeature: Sample[BlockchainFeature] = mk(BlockchainFeature(777, "The most stronger feature"))
}
