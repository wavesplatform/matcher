package com.wavesplatform.dex.actors

import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => DrivenPropertyChecks}

class SnapshotStateSpecification extends AnyPropSpecLike with DrivenPropertyChecks with Matchers with NoShrink {
  property("nextSnapshotOffset generates greater offsets than old and last processed") {
    val assetPair = AssetPair(
      IssuedAsset(ByteStr("asset1".getBytes())),
      IssuedAsset(ByteStr("asset2".getBytes()))
    )

    val g = for {
      interval            <- Gen.choose(1, 1000L).label("interval")
      currSnapshotOffset  <- Gen.choose(-1, 1000L).label("currSnapshotOffset")
      lastProcessedOffset <- Gen.choose(-1, 1000L).label("lastProcessedOffset")
    } yield (currSnapshotOffset, lastProcessedOffset, interval)

    forAll(g) {
      case (currSnapshotOffset, lastProcessedOffset, interval) =>
        val nextOffset = SnapshotsState.nextSnapshotOffset(assetPair, currSnapshotOffset, lastProcessedOffset, interval)
        nextOffset should be > currSnapshotOffset
        nextOffset should be > lastProcessedOffset
    }
  }
}
