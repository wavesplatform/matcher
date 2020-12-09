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
import com.wavesplatform.dex.test.matchers.ProduceError.produce
import com.wavesplatform.dex.{NoShrink, WavesIntegrationSuiteBase}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.Queue
import scala.util.matching.Regex

class WavesChainTestSuite extends WavesIntegrationSuiteBase with ScalaCheckDrivenPropertyChecks with NoShrink {

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

  private val emptyChain = Queue.empty[WavesBlock]

  "WavesChain" - {
    "dropLiquidBlock" - {
      "empty" in {
        WavesChain.dropLiquidBlock(block1, emptyChain) should matchTo((List.empty[WavesBlock], emptyChain))
      }

      "positive" - {
        def testGen(blocksNumber: Range, microBlocksNumber: Range): Gen[(WavesBlock, Queue[WavesBlock])] =
          chainGen(blocksNumber, microBlocksNumber).map { chain =>
            val headBlock = chain.headOption
            (
              WavesBlock(
                ref = BlockRef(headBlock.fold(1)(_.ref.height + 1), ByteStr(Array[Byte](-7))),
                reference = headBlock.fold(ByteStr(Array[Byte](-8)))(_.ref.id),
                changes = Monoid.empty[BlockchainBalance],
                tpe = WavesBlock.Type.FullBlock
              ),
              chain
            )
          }

        "liquid block is not empty" - {
          "the last liquid block's part is referenced by a new block" in forAll(testGen(1 to 3, 1 to 3)) { case (newBlock, chain) =>
            val (liquidBlock, _) = WavesChain.dropLiquidBlock(newBlock, chain)
            liquidBlock.last.ref.id shouldBe newBlock.reference
          }

          "the rest history should not contain a referenced block" in forAll(testGen(1 to 3, 1 to 3)) { case (newBlock, chain) =>
            val (_, restHistory) = WavesChain.dropLiquidBlock(newBlock, chain)
            restHistory.find(_.ref.id == newBlock.reference) shouldBe empty
          }
        }

        "liquid block is empty" - {
          "when there are no micro blocks" in forAll(testGen(1 to 3, 0 to 0)) { case (newBlock, chain) =>
            val (liquidBlock, _) = WavesChain.dropLiquidBlock(newBlock, chain)
            liquidBlock shouldBe empty
          }

          "the head block in the rest history is referenced by a new one" in forAll(testGen(1 to 3, 0 to 0)) { case (newBlock, chain) =>
            val (_, restHistory) = WavesChain.dropLiquidBlock(newBlock, chain)
            restHistory.head.ref.id shouldBe newBlock.reference
          }
        }
      }
    }

    "dropDifference" - {
      val testGen = for {
        commonBlocks <- Gen.const(mkChain(1, 0)) // chainGen(0 to 2, 0 to 2)
        (maxBlocksNumber, startHeight) = commonBlocks.headOption match {
          case Some(lastBlock) =>
            if (lastBlock.tpe == WavesBlock.Type.FullBlock) (2, lastBlock.ref.height + 1)
            else (0, lastBlock.ref.height)
          case _ => (2, 0)
        }

        detachedChain1 <- chainGen(0 to maxBlocksNumber, 0 to 2, startHeight)
        detachedChain2 <- chainGen(0 to maxBlocksNumber, 0 to 2, startHeight)
      } yield {
        val chain1 = detachedChain1.appendedAll(commonBlocks)
        val chain2 = detachedChain2.appendedAll(commonBlocks)
        (
          commonBlocks,
          WavesChain(chain1, chain1.headOption.fold(0)(_.ref.height), 100),
          WavesChain(chain2, chain2.headOption.fold(0)(_.ref.height), 100)
        )
      }

      "there is no common block between dropped" in forAll(testGen) { case (commonBlocks, chain1, chain2) =>
        val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
        commonBlocks.foreach { commonBlock =>
          withClue("dropped1: ") {
            dropped1 should not contain commonBlock
          }
          withClue("dropped2: ") {
            dropped2 should not contain commonBlock
          }
        }
      }
    }

    // Terminology:
    // * block - full block or micro block
    // Properties:
    // 1. If we have no blocks and receive a full block, this block is added in the front of the current fork
    // 2. If we have no blocks and receive a micro block, we restart from the current height
    // 3. If we have no micro blocks and receive a full block that is referenced to the last known block, this block is added in the front of the current fork
    // 4. If we have micro blocks and receive a full block that is referenced to the last known full block or its micro blocks
    //    1. The last block and its micro blocks are replaced by a hardened block in the fork
    //    2. See 3
    // 5. If we receive an irrelevant block, we drop the last full block and its micro blocks [and restart the stream from the last block's height]

    "withBlock" - {
      "empty +" - {
        val init = WavesChain(emptyChain, 0, 100)

        "block" in { init.withBlock(block1) should matchTo(WavesChain(Queue(block1), 1, 99).asRight[String]) }

        "micro block" in {
          val microBlock = block1.copy(tpe = WavesBlock.Type.MicroBlock)
          init.withBlock(microBlock) should produce("(?s)^Can't attach a micro block.+to empty chain$".r)
        }
      }

      "block +" - {
        val init = WavesChain(Queue(block1), 1, 99)
        "expected" - {
          "block" in { init.withBlock(block2) should matchTo(WavesChain(Queue(block2, block1), 2, 98).asRight[String]) }

          "micro block" in {
            val microBlock = block2.copy(
              ref = BlockRef(height = 1, id = block2.ref.id),
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock) should matchTo(WavesChain(Queue(microBlock, block1), 1, 99).asRight[String])
          }
        }

        "unexpected" - {
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit = init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must be after.+".r, _.copy(reference = ByteStr.empty))

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

        val init = WavesChain(Queue(microBlock1, block1), 1, 99)

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
                tpe = WavesBlock.Type.FullBlock
              )

              val hardenedBlock = block1.copy(
                ref = microBlock1.ref,
                reference = block1.reference,
                changes = block1.changes |+| microBlock1.changes,
                tpe = WavesBlock.Type.FullBlock
              )

              init.withBlock(newBlock) should matchTo(WavesChain(Queue(newBlock, hardenedBlock), 2, 98).asRight[String])
            }
          }

          "micro block" in {
            val microBlock2 = block2.copy(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 2))),
              reference = microBlock1.ref.id,
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock2) should matchTo(WavesChain(Queue(microBlock2, microBlock1, block1), 1, 99).asRight[String])
          }
        }

        "unexpected" - {
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit =
            init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must be after.+".r, _.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test("(?s)^The new block.+must be after.+".r, x => x.copy(ref = x.ref.copy(height = h)))
              "1" in heightTest(1)
              "3" in heightTest(3)
            }

            "referenced to a key block" in {
              val newBlock = WavesBlock(
                ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1))),
                reference = block1.ref.id,
                changes = BlockchainBalance( // TODO changes here are not essential
                  regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                  outLeases = Map(alice -> 1L)
                ),
                tpe = WavesBlock.Type.FullBlock
              )

              init.withBlock(newBlock) should produce("(?s)^The new block.+must be after.+".r)
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

        val init = WavesChain(Queue(microBlock2, microBlock1, block1), 1, 99)

        "unexpected" - {
          "block referenced to the previous micro block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 2))),
              reference = microBlock1.ref.id,
              changes = BlockchainBalance(
                regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                outLeases = Map(alice -> 1L)
              ),
              tpe = WavesBlock.Type.FullBlock
            )

            init.withBlock(newBlock) should produce("(?s)^The new block.+must be after.+".r)
          }

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

        val init = WavesChain(Queue(microBlock, block2, block1), 2, 98)

        val newBlock = WavesBlock(
          ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 1, 0))),
          reference = block1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 35)),
            outLeases = Map.empty
          ),
          tpe = WavesBlock.Type.FullBlock
        )

        init.withBlock(newBlock) should produce("(?s)^The new block.+must be after.+".r)
      }
    }
  }

  private def chainGen(blocksNumber: Range, microBlocksNumber: Range, startHeight: Int = 0): Gen[Queue[WavesBlock]] = for {
    blocksNumber <- Gen.choose(blocksNumber.head, blocksNumber.last)
    microBlocksNumber <- if (blocksNumber == 0) Gen.const(0) else Gen.choose(microBlocksNumber.head, microBlocksNumber.last)
  } yield mkChain(blocksNumber, microBlocksNumber, startHeight)

  private def mkChain(blocksNumber: Int, microBlocksNumber: Int, startHeight: Int = 0): Queue[WavesBlock] =
    if (blocksNumber <= 0) Queue.empty
    else {
      val initBlock = WavesBlock(
        ref = BlockRef(startHeight, ByteStr(Array[Byte](0))),
        reference = ByteStr.empty,
        changes = Monoid.empty[BlockchainBalance],
        tpe = WavesBlock.Type.FullBlock
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
        .to(Queue)
        .prepended(initBlock) // 1, 2, 3

      val microBlocks = blocks.lastOption match {
        case None => List.empty[WavesBlock]
        case Some(lastBlock) =>
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

      blocks.appendedAll(microBlocks).reverse // 5, 4, 3, 2, 1
    }

}
