package com.wavesplatform.dex.grpc.integration.clients.domain

import java.nio.charset.StandardCharsets

import cats.Monoid
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesFork.Status
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class WavesForkTestSuite extends WavesIntegrationSuiteBase with ScalaCheckDrivenPropertyChecks with NoShrink {

  private val alice = KeyPair(ByteStr("alice".getBytes(StandardCharsets.UTF_8))).toAddress
  private val bob = KeyPair(ByteStr("bob".getBytes(StandardCharsets.UTF_8))).toAddress

  private val usd = IssuedAsset(Base58.decode("usd"))

  private val block1 = WavesBlock(
    ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0))),
    reference = ByteStr.empty,
    changes = BlockchainBalance(
      regular = Map(alice -> Map(Waves -> 10, usd -> 2L)),
      outLeases = Map(bob -> 23L)
    ),
    tpe = WavesBlock.Type.FullBlock
  )

  private val block2 = WavesBlock(
    ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
    reference = block1.ref.id,
    changes = BlockchainBalance(
      regular = Map(bob -> Map(usd -> 35)),
      outLeases = Map.empty
    ),
    tpe = WavesBlock.Type.FullBlock
  )

  private val block3 = WavesBlock(
    ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 1, 1, 0))),
    reference = block2.ref.id,
    changes = BlockchainBalance(
      regular = Map(alice -> Map(usd -> 30)),
      outLeases = Map(alice -> 1L)
    ),
    tpe = WavesBlock.Type.FullBlock
  )

  "WavesFork" - {
    "withBlock" - {
      "Failed - on an unexpected block, removes the last block" in {
        val fork = WavesFork(
          WavesChain(Vector(block2, block1), 98),
          WavesChain(Vector(block1), 98)
        )

        val invalidBlock = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
          reference = ByteStr(Array[Byte](-98)),
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 35)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.FullBlock
        )

        val expectedUpdatedFork = WavesFork(
          WavesChain(Vector(block2, block1), 98),
          WavesChain(Vector.empty, 99)
        )

        fork.withBlock(invalidBlock) should matchTo(Status.Failed(
          updatedFork = expectedUpdatedFork,
          reason = "The new block BlockRef(h=2, ZvGf) (reference=3j) must be after BlockRef(h=1, 8TZ)"
        ): Status)
      }

      "NotResolved - the height is still less than on the previous chain" - {
        "a full block" in {
          val fork = WavesFork(
            WavesChain(Vector(block2, block1), 98),
            WavesChain(Vector(block1), 99)
          )

          val expectedUpdatedFork = WavesFork(
            WavesChain(Vector(block2, block1), 98),
            WavesChain(Vector(block2, block1), 98)
          )

          fork.withBlock(block2) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }

        "a micro block with a lesser key block height than in the original chain" in {
          val fork = WavesFork(
            WavesChain(Vector(block3, block2, block1), 97),
            WavesChain(Vector(block2, block1), 98)
          )

          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
            reference = block2.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 30)),
              outLeases = Map(alice -> 1L)
            ),
            tpe = WavesBlock.Type.MicroBlock
          )

          val expectedUpdatedFork = WavesFork(
            WavesChain(Vector(block3, block2, block1), 97),
            WavesChain(Vector(microBlock, block2, block1), 98)
          )

          fork.withBlock(microBlock) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }

        "an existed micro block on the same chain" in {
          val microBlock1 = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 2))),
            reference = block1.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 69)),
              outLeases = Map(bob -> 2L)
            ),
            tpe = WavesBlock.Type.MicroBlock
          )

          val fork = WavesFork(
            WavesChain(Vector(microBlock1, block1), 99),
            WavesChain(Vector(block1), 99)
          )

          val expectedUpdatedFork = WavesFork(
            WavesChain(Vector(microBlock1, block1), 99),
            WavesChain(Vector(microBlock1, block1), 99)
          )

          fork.withBlock(microBlock1) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }
      }

      "Resolved on a micro block" - {
        "after the key block" in {
          val fork = WavesFork(
            WavesChain(Vector(block2, block1), 98),
            WavesChain(Vector(block2, block1), 98)
          )

          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
            reference = block2.ref.id,
            changes = BlockchainBalance(
              regular = Map(bob -> Map(usd -> 31)),
              outLeases = Map(bob -> 10L)
            ),
            tpe = WavesBlock.Type.MicroBlock
          )

          fork.withBlock(microBlock) should matchTo(Status.Resolved(
            activeChain = WavesChain(Vector(microBlock, block2, block1), 98),
            newChanges = BlockchainBalance(
              regular = Map(bob -> Map(usd -> 31)),
              outLeases = Map(bob -> 10L)
            ),
            lostDiffIndex = Monoid.empty[DiffIndex]
          ): Status)
        }

        "unknown" in {
          val microBlock1 = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 2))),
            reference = block1.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 69)),
              outLeases = Map(bob -> 2L)
            ),
            tpe = WavesBlock.Type.MicroBlock
          )

          val microBlock2 = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 3))),
            reference = microBlock1.ref.id,
            changes = BlockchainBalance(
              regular = Map(bob -> Map(Waves -> 11)),
              outLeases = Map(alice -> 9L)
            ),
            tpe = WavesBlock.Type.MicroBlock
          )

          val fork = WavesFork(
            WavesChain(Vector(microBlock1, block1), 99),
            WavesChain(Vector(microBlock1, block1), 99)
          )

          fork.withBlock(microBlock2) should matchTo(Status.Resolved(
            activeChain = WavesChain(Vector(microBlock2, microBlock1, block1), 99),
            newChanges = microBlock2.changes,
            lostDiffIndex = Monoid.empty[DiffIndex]
          ): Status)
        }
      }
    }
    // "withoutLast" - {} // No need, delegates to WavesChain
    // "rollbackTo" - {} // No need, delegates to WavesChain
  }

}
