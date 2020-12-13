package com.wavesplatform.dex.grpc.integration.clients.domain

import java.nio.charset.StandardCharsets

import cats.Monoid
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesFork.Status
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.matching.Regex

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
        "full block" in {
          val fork = WavesFork(
            WavesChain(Vector(block3, block2, block1), 97),
            WavesChain(Vector(block1), 99)
          )

          val expectedUpdatedFork = WavesFork(
            WavesChain(Vector(block3, block2, block1), 97),
            WavesChain(Vector(block2, block1), 98)
          )

          fork.withBlock(block2) should matchTo(Status.NotResolved(expectedUpdatedFork): Status)
        }

        "micro block" in {
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
      }

      "Resolved on micro block" in {
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
    }
    // "withoutLast" - {} // No need, delegates to WavesChain
    // "rollbackTo" - {} // No need, delegates to WavesChain
  }

}
