package com.wavesplatform.dex.error

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import scala.util.Try

@FunctionalInterface
trait ErrorFormatterContext {
  def assetDecimals(asset: Asset): Option[Int]
  def unsafeAssetDecimals(asset: Asset): Int = assetDecimals(asset) match {
    case Some(value) => value
    case None        => throw new RuntimeException(s"Can't find asset: $asset")
  }
}

object ErrorFormatterContext {
  def from(f: Asset => Int): ErrorFormatterContext = new ErrorFormatterContext {
    override def assetDecimals(asset: Asset): Option[Int] = Try(f(asset)).fold(_ => None, _.some)
    override def unsafeAssetDecimals(asset: Asset): Int   = f(asset)
  }

  def fromOptional(f: Asset => Option[Int]): ErrorFormatterContext = (asset: Asset) => f(asset)
  def from(xs: PartialFunction[Asset, Int]): ErrorFormatterContext = fromOptional(xs.lift)
}
