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
      outgoingLeasing = Map(bob -> 23L)
    ),
    tpe = WavesBlock.Type.FullBlock,
    confirmedTxs = mkTransactionWithChangesMap(1)
  )

  private val block2 = WavesBlock(
    ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
    reference = block1.ref.id,
    changes = BlockchainBalance(
      regular = Map(bob -> Map(usd -> 35)),
      outgoingLeasing = Map.empty
    ),
    tpe = WavesBlock.Type.FullBlock,
    confirmedTxs = mkTransactionWithChangesMap(2)
  )

  private val block3 = WavesBlock(
    ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 1, 1, 0))),
    reference = block2.ref.id,
    changes = BlockchainBalance(
      regular = Map(alice -> Map(usd -> 30)),
      outgoingLeasing = Map(alice -> 1L)
    ),
    tpe = WavesBlock.Type.FullBlock,
    confirmedTxs = mkTransactionWithChangesMap(3)
  )

  "WavesFork" - {
    "withBlock" - {
      "Failed - on an unexpected block, removes the last block" in {
        val fork = WavesFork(
          mkChain(Vector(block2, block1), 98),
          mkChain(Vector(block1), 98)
        )

        val invalidBlock = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
          reference = ByteStr(Array[Byte](-98)),
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 35)),
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.FullBlock,
          confirmedTxs = mkTransactionWithChangesMap(10)
        )

        val expectedUpdatedFork = WavesFork(
          mkChain(Vector(block2, block1), 98),
          WavesChain(Vector.empty, block1.ref.height - 1, 99)
        )

        fork.withBlock(invalidBlock) should matchTo(Status.Failed(
          updatedFork = expectedUpdatedFork,
          reason = "The new block Ref(h=2, ZvGf) (reference=3j) must be after Ref(h=1, 8TZ)"
        ): Status)
      }

      "NotResolved - the height is still less or equal to the previous chain" - {
        "a full block" in {
          val fork = WavesFork(
            mkChain(Vector(block2, block1), 98),
            mkChain(Vector(block1), 99)
          )

          val expectedUpdatedFork = WavesFork(
            mkChain(Vector(block2, block1), 98),
            mkChain(Vector(block2, block1), 98)
          )

          fork.withBlock(block2) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }

        "a micro block with a lesser key block height than in the original chain" in {
          val fork = WavesFork(
            mkChain(Vector(block3, block2, block1), 97),
            mkChain(Vector(block2, block1), 98)
          )

          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
            reference = block2.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 30)),
              outgoingLeasing = Map(alice -> 1L)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            confirmedTxs = mkTransactionWithChangesMap(10)
          )

          val expectedUpdatedFork = WavesFork(
            mkChain(Vector(block3, block2, block1), 97),
            mkChain(Vector(microBlock, block2, block1), 98)
          )

          fork.withBlock(microBlock) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }

        "an existed micro block on the same chain" in {
          val microBlock1 = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 2))),
            reference = block1.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 69)),
              outgoingLeasing = Map(bob -> 2L)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            confirmedTxs = mkTransactionWithChangesMap(10)
          )

          val fork = WavesFork(
            mkChain(Vector(microBlock1, block1), 99),
            mkChain(Vector(block1), 99)
          )

          val expectedUpdatedFork = WavesFork(
            mkChain(Vector(microBlock1, block1), 99),
            mkChain(Vector(microBlock1, block1), 99)
          )

          fork.withBlock(microBlock1) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }
      }

      "Resolved" - {
        "on a key block with a higher height" in {
          val block4 = WavesBlock(
            ref = BlockRef(height = 4, id = ByteStr(Array[Byte](98, 1, 1, 1, 0))),
            reference = block3.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(Waves -> 10)),
              outgoingLeasing = Map(bob -> 10L)
            ),
            tpe = WavesBlock.Type.FullBlock,
            confirmedTxs = mkTransactionWithChangesMap(4)
          )

          val fork = WavesFork(
            mkChain(Vector(block2, block1), 98),
            mkChain(Vector(block3, block2, block1), 97)
          )

          fork.withBlock(block4) should matchTo(Status.Resolved(
            activeChain = mkChain(Vector(block4, block3, block2, block1), 96),
            newChanges = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 30L, Waves -> 10)),
              outgoingLeasing = Map(alice -> 1L, bob -> 10)
            ),
            lostDiffIndex = Monoid.empty[DiffIndex],
            lostTxIds = Map.empty,
            confirmedTxs = block3.confirmedTxs ++ block4.confirmedTxs
          ): Status)
        }

        "on a micro block" - {
          "after the key block with the same height (same blocks)" in {
            val fork = WavesFork(
              mkChain(Vector(block2, block1), 98),
              mkChain(Vector(block2, block1), 98)
            )

            val microBlock = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
              reference = block2.ref.id,
              changes = BlockchainBalance(
                regular = Map(bob -> Map(usd -> 31)),
                outgoingLeasing = Map(bob -> 10L)
              ),
              tpe = WavesBlock.Type.MicroBlock,
              confirmedTxs = mkTransactionWithChangesMap(10)
            )

            fork.withBlock(microBlock) should matchTo(Status.Resolved(
              activeChain = mkChain(Vector(microBlock, block2, block1), 98),
              newChanges = BlockchainBalance(
                regular = Map(bob -> Map(usd -> 31)),
                outgoingLeasing = Map(bob -> 10L)
              ),
              lostDiffIndex = Monoid.empty[DiffIndex],
              lostTxIds = Map.empty,
              confirmedTxs = microBlock.confirmedTxs
            ): Status)
          }

          "after the micro block with the same height (same blocks and micro blocks)" in {
            val microBlock1 = WavesBlock(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 2))),
              reference = block1.ref.id,
              changes = BlockchainBalance(
                regular = Map(alice -> Map(usd -> 69)),
                outgoingLeasing = Map(bob -> 2L)
              ),
              tpe = WavesBlock.Type.MicroBlock,
              confirmedTxs = mkTransactionWithChangesMap(10)
            )

            val microBlock2 = WavesBlock(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 3))),
              reference = microBlock1.ref.id,
              changes = BlockchainBalance(
                regular = Map(bob -> Map(Waves -> 11)),
                outgoingLeasing = Map(alice -> 9L)
              ),
              tpe = WavesBlock.Type.MicroBlock,
              confirmedTxs = mkTransactionWithChangesMap(11)
            )

            val fork = WavesFork(
              mkChain(Vector(microBlock1, block1), 99),
              mkChain(Vector(microBlock1, block1), 99)
            )

            fork.withBlock(microBlock2) should matchTo(Status.Resolved(
              activeChain = mkChain(Vector(microBlock2, microBlock1, block1), 99),
              newChanges = microBlock2.changes,
              lostDiffIndex = Monoid.empty[DiffIndex],
              lostTxIds = Map.empty,
              confirmedTxs = microBlock2.confirmedTxs
            ): Status)
          }
        }
      }
    }
    // "withoutLast" - {} // No need, delegates to WavesChain
    // "rollbackTo" - {} // No need, delegates to WavesChain
  }

}
