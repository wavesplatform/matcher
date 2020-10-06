package com.wavesplatform.dex

import org.scalacheck.{Arbitrary, Gen}

package object gen {
  val bytes32gen: Gen[Array[Byte]] = byteArrayGen(32)
  val issuedAssetIdGen: Gen[Array[Byte]] = bytes32gen
  def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.containerOfN[Array, Byte](length, Arbitrary.arbitrary[Byte])
}
