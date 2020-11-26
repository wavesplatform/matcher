package com.wavesplatform.dex.grpc.integration.clients.status

import java.nio.charset.StandardCharsets

import cats.Monoid
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import org.scalacheck.{Gen, Shrink}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.matching.Regex

class WavesForkTestSuite extends WavesIntegrationSuiteBase with ScalaCheckDrivenPropertyChecks {

  // NoShrink
  implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

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
    tpe = WavesBlock.Type.Block
  )

  private val block2 = WavesBlock(
    ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
    reference = block1.ref.id,
    changes = BlockchainBalance(
      regular = Map(bob -> Map(usd -> 35)),
      outLeases = Map.empty
    ),
    tpe = WavesBlock.Type.Block
  )

  private def chainGen(blocksNumber: Range, microBlocksNumber: Range): Gen[List[WavesBlock]] = for {
    blocksNumber <- Gen.choose(blocksNumber.head, blocksNumber.last)
    microBlocksNumber <- if (blocksNumber == 0) Gen.const(0) else Gen.choose(microBlocksNumber.head, microBlocksNumber.last)
  } yield {
    val initBlock = WavesBlock(
      ref = BlockRef(0, ByteStr(Array[Byte](0))),
      reference = ByteStr.empty,
      changes = Monoid.empty[BlockchainBalance],
      tpe = WavesBlock.Type.Block
    )

    val blocks = Iterator
      .unfold(initBlock) { prev =>
        val next = prev.copy(
          ref = BlockRef(prev.ref.height + 1, ByteStr(prev.ref.id.arr.prepended(1))),
          reference = prev.ref.id
        )
        (next, next).some
      }
      .take(blocksNumber)
      .toList
      .reverse // 3, 2, 1

    val microBlocks = blocks match {
      case Nil => List.empty[WavesBlock]
      case lastBlock :: _ =>
        Iterator
          .unfold(lastBlock) { prev =>
            val next = prev.copy(
              ref = prev.ref.copy(id = ByteStr(prev.ref.id.arr.prepended(2))), // height remains
              reference = prev.ref.id,
              tpe = WavesBlock.Type.MicroBlock
            )
            (next, next).some
          }
          .take(microBlocksNumber)
          .toList // 4, 5
    }

    blocks.reverse_:::(microBlocks) // 5, 4, 3, 2, 1
  }

  "WavesFork" - {
    "dropPreviousFork" - {
      "empty" in {
        val emptyChain = List.empty[WavesBlock]
        WavesFork.dropPreviousFork(block1, emptyChain) should matchTo((emptyChain, emptyChain).asRight[String])
      }

      "positive" - {
        val testGen: Gen[(WavesBlock, List[WavesBlock])] =
          for {
            chain <- chainGen(
              blocksNumber = 1 to 1,
              microBlocksNumber = 0 to 3
            )
            referencedBlock <- Gen.choose(0, chain.length - 1).map(chain.apply)
          } yield (
            WavesBlock(
              ref = BlockRef(referencedBlock.ref.height + 1, ByteStr(Array[Byte](-3))),
              reference = referencedBlock.ref.id,
              changes = Monoid.empty[BlockchainBalance],
              tpe = WavesBlock.Type.Block
            ),
            chain
          )

        def test(f: (WavesBlock, List[WavesBlock], List[WavesBlock]) => Any): Unit = forAll(testGen) { case (newBlock, chain) =>
          WavesFork.dropPreviousFork(newBlock, chain) match {
            case Left(e) => fail(e)
            case Right((dropped, restChain)) => f(newBlock, dropped, restChain)
          }
        }

        "there is no dropped block that is referenced by a new one" in test { (newBlock, dropped, _) =>
          dropped.find(_.ref.id == newBlock.reference) shouldBe empty
        }

        "the cut chain is not empty" in test { (_, _, restChain) =>
          restChain shouldNot be(empty)
        }

        "the new block references the first block in a cut chain" in test { (newBlock, _, restChain) =>
          newBlock.reference shouldBe restChain.head.ref.id
        }
      }

      "negative" - {
        val testGen: Gen[(WavesBlock, List[WavesBlock])] =
          for {
            chain <- chainGen(
              blocksNumber = 1 to 1,
              microBlocksNumber = 0 to 3
            )
          } yield (
            WavesBlock(
              ref = BlockRef(chain.head.ref.height + 1, ByteStr(Array(-3))),
              reference = ByteStr(Array(-2)),
              changes = Monoid.empty[BlockchainBalance],
              tpe = WavesBlock.Type.Block
            ),
            chain
          )

        "returns an error if referenced an unknown block" in forAll(testGen) { case (newBlock, chain) =>
          WavesFork.dropPreviousFork(newBlock, chain) should produce("(?s)^The new block.+ must reference.+$".r)
        }
      }
    }

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
        val init = WavesFork(List(block1))
        "expected" - {
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
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit = init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must reference.+".r, _.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test("(?s)^The new block.+must be after.+".r, x => x.copy(ref = x.ref.copy(height = h)))
              "1" in heightTest(1)
              "3" in heightTest(3)
            }
          }

          "micro block" - {
            "unexpected reference" in test(
              "(?s)^The new micro block.+must reference.+".r,
              _.copy(
                tpe = WavesBlock.Type.MicroBlock,
                reference = ByteStr.empty
              )
            )

            "unexpected height" - {
              def heightTest(h: Int): Unit = test(
                "(?s)^The new micro block.+must reference.+".r,
                x => x.copy(tpe = WavesBlock.Type.MicroBlock, ref = x.ref.copy(height = h))
              )
              "1" in heightTest(0)
              "3" in heightTest(2)
            }
          }
        }
      }

      "block, micro block +" - {
        val microBlock1 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 1))),
          reference = block1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 7), alice -> Map(usd -> 24)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock
        )

        val init = WavesFork(List(microBlock1, block1))

        "expected" - {
          "block referenced to the" - {
            "micro block" in {
              val newBlock = WavesBlock(
                ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1))),
                reference = microBlock1.ref.id,
                changes = BlockchainBalance(
                  regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                  outLeases = Map(alice -> 1L)
                ),
                tpe = WavesBlock.Type.Block
              )

              val hardenedBlock = block1.copy(
                ref = microBlock1.ref,
                reference = block1.reference,
                changes = block1.changes |+| microBlock1.changes,
                tpe = WavesBlock.Type.Block
              )

              init.withBlock(newBlock) should matchTo(WavesFork(List(newBlock, hardenedBlock)).asRight[String])
            }

            "block" in {
              val newBlock = WavesBlock(
                ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1))),
                reference = block1.ref.id,
                changes = BlockchainBalance( // TODO changes here are not essential
                  regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                  outLeases = Map(alice -> 1L)
                ),
                tpe = WavesBlock.Type.Block
              )

              init.withBlock(newBlock) should matchTo(WavesFork(List(newBlock, block1)).asRight[String])
            }
          }

          "micro block" in {
            val microBlock2 = block2.copy(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 2))),
              reference = microBlock1.ref.id,
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock2) should matchTo(WavesFork(List(microBlock2, microBlock1, block1)).asRight[String])
          }
        }

        "unexpected" - {
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit =
            init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must reference.+".r, _.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test("(?s)^The new block.+must be after.+".r, x => x.copy(ref = x.ref.copy(height = h)))
              "1" in heightTest(1)
              "3" in heightTest(3)
            }
          }

          "micro block" - {
            "unexpected reference" in test(
              "(?s)^The new micro block.+must reference.+".r,
              _.copy(
                tpe = WavesBlock.Type.MicroBlock,
                reference = ByteStr.empty
              )
            )

            "unexpected height" - {
              def heightTest(h: Int): Unit = test(
                "(?s)^The new micro block.+must reference.+".r,
                x => x.copy(tpe = WavesBlock.Type.MicroBlock, ref = x.ref.copy(height = h))
              )
              "1" in heightTest(0)
              "3" in heightTest(2)
            }
          }
        }
      }

      "block, micro block, micro block +" - {
        val microBlock1 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 1))),
          reference = block1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 7), alice -> Map(usd -> 24)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock
        )

        val microBlock2 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 2))),
          reference = microBlock1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 3), alice -> Map(usd -> 11)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock
        )

        val init = WavesFork(List(microBlock2, microBlock1, block1))

        "expected" - {
          "block referenced to the previous micro block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 2))),
              reference = microBlock1.ref.id,
              changes = BlockchainBalance(
                regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                outLeases = Map(alice -> 1L)
              ),
              tpe = WavesBlock.Type.Block
            )

            val hardenedBlock = block1.copy(
              ref = microBlock1.ref,
              reference = block1.reference,
              changes = block1.changes |+| microBlock1.changes,
              tpe = WavesBlock.Type.Block
            )

            init.withBlock(newBlock) should matchTo(WavesFork(List(newBlock, hardenedBlock)).asRight[String])
          }
        }

        "unexpected" - {
          def test(updateNext: WavesBlock => WavesBlock): Unit =
            WavesFork(List(block1)).withBlock(updateNext(block2)) should produce("(?s)^A new.+block.+must continue the chain.+".r)

          "micro block referenced to the previous micro block" - {
            "unexpected reference" in {
              val microBlock3 = WavesBlock(
                ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 3))),
                reference = microBlock1.ref.id,
                changes = BlockchainBalance(
                  regular = Map(bob -> Map(usd -> 3), alice -> Map(usd -> 11)),
                  outLeases = Map.empty
                ),
                tpe = WavesBlock.Type.MicroBlock
              )

              init.withBlock(microBlock3) should produce("(?s)^The new micro block.+must reference the last block.+".r)
            }
          }
        }
      }

      "block, block, micro block + block referenced to the previous one" in {
        val microBlock = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 0, 1))),
          reference = block2.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 7), alice -> Map(usd -> 24)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock
        )

        val init = WavesFork(List(microBlock, block2, block1))

        val newBlock = WavesBlock(
          ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 1, 0))),
          reference = block1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 35)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.Block
        )

        init.withBlock(newBlock) should produce("(?s)^The new block.+must reference.+the one of.+".r)
      }
    }
  }
}
