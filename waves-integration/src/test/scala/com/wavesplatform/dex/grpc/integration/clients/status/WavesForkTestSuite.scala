package com.wavesplatform.dex.grpc.integration.clients.status

import java.nio.charset.StandardCharsets

import cats.syntax.either._
import cats.syntax.semigroup._
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.ProduceError.produce

class WavesForkTestSuite extends WavesIntegrationSuiteBase {

  private val alice = KeyPair(ByteStr("alice".getBytes(StandardCharsets.UTF_8))).toAddress
  private val bob = KeyPair(ByteStr("bob".getBytes(StandardCharsets.UTF_8))).toAddress

  private val usd = IssuedAsset(Base58.decode("usd"))

  private val updatedBalances1 = BlockchainBalance(
    regular = Map(alice -> Map(Waves -> 10, usd -> 2L)),
    outLeases = Map(bob -> 23L)
  )

  private val updatedBalances2 = BlockchainBalance(
    regular = Map(bob -> Map(usd -> 35)),
    outLeases = Map.empty
  )

  private val block1 = WavesBlock(
    ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
    reference = ByteStr.empty,
    changes = updatedBalances1,
    tpe = WavesBlock.Type.Block
  )

  private val block2 = WavesBlock(
    ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3, 4))),
    reference = block1.ref.id,
    changes = updatedBalances2,
    tpe = WavesBlock.Type.Block
  )

  "WavesFork" - {
    "withBlock" - {
      "empty +" - {
        val init = WavesFork(List.empty)

        "block" in { init.withBlock(block1) should matchTo(WavesFork(List(block1)).asRight[String]) }

        "micro block" in {
          val microBlock = block1.copy(tpe = WavesBlock.Type.MicroBlock)
          init.withBlock(microBlock) should produce("(?s)^Can't attach a micro block.+to empty chain$".r)
        }
      }

      "block +" - {
        "expected" - {
          val init = WavesFork(List(block1))

          "block" in { init.withBlock(block2) should matchTo(WavesFork(List(block2, block1)).asRight[String]) }

          "micro block" in {
            val microBlock = block2.copy(
              ref = BlockRef(height = 1, id = block2.ref.id),
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock) should matchTo(WavesFork(List(microBlock, block1)).asRight[String])
          }
        }

        "unexpected" - {
          def test(updateNext: WavesBlock => WavesBlock): Unit =
            WavesFork(List(block1)).withBlock(updateNext(block2)) should produce("(?s)^A new.+block.+must continue the chain.+".r)

          "block" - {
            "unexpected reference" in test(_.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test(x => x.copy(ref = x.ref.copy(height = h)))
              "1" in heightTest(1)
              "3" in heightTest(3)
            }
          }

          "micro block" - {
            "unexpected reference" in test(_.copy(
              tpe = WavesBlock.Type.MicroBlock,
              reference = ByteStr.empty
            ))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test(x => x.copy(tpe = WavesBlock.Type.MicroBlock, ref = x.ref.copy(height = h)))
              "1" in heightTest(0)
              "3" in heightTest(2)
            }
          }
        }
      }
    }
  }
}
