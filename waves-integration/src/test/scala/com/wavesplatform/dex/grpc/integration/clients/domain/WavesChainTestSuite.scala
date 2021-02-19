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

  private val defaultChainGen = for {
    history <- historyGen(1 to 3, 0 to 2, 0 to 2)
    capacity <- Gen.choose(0, 2)
  } yield mkChain(history, capacity)

  private val emptyChain = Vector.empty[WavesBlock]

  "WavesChain" - {
    "dropLiquidBlock" - {
      "empty" in {
        WavesChain.dropLiquidBlock(block1, emptyChain) should matchTo((List.empty[WavesBlock], emptyChain))
      }

      "positive" - {
        def testGen(blocksNumber: Range, microBlocksNumber: Range): Gen[(WavesBlock, Vector[WavesBlock])] =
          historyGen(blocksNumber, microBlocksNumber).map { history =>
            val headBlock = history.headOption
            (
              WavesBlock(
                ref = BlockRef(headBlock.fold(1)(_.ref.height + 1), ByteStr(Array[Byte](-7))),
                reference = headBlock.fold(ByteStr(Array[Byte](-8)))(_.ref.id),
                changes = Monoid.empty[BlockchainBalance],
                tpe = WavesBlock.Type.FullBlock,
                confirmedTxs = Map.empty
              ),
              history
            )
          }

        "liquid block is not empty" - {
          "the last liquid block's part is referenced by a new block" in forAll(testGen(1 to 3, 1 to 3)) { case (newBlock, history) =>
            val (liquidBlock, _) = WavesChain.dropLiquidBlock(newBlock, history)
            liquidBlock.last.ref.id shouldBe newBlock.reference
          }

          "the rest history should not contain a referenced block" in forAll(testGen(1 to 3, 1 to 3)) { case (newBlock, history) =>
            val (_, restHistory) = WavesChain.dropLiquidBlock(newBlock, history)
            restHistory.find(_.ref.id == newBlock.reference) shouldBe empty
          }
        }

        "liquid block is empty" - {
          "when there are no micro blocks" in forAll(testGen(1 to 3, 0 to 0)) { case (newBlock, history) =>
            val (liquidBlock, _) = WavesChain.dropLiquidBlock(newBlock, history)
            liquidBlock shouldBe empty
          }

          "the head block in the rest history is referenced by a new one" in forAll(testGen(1 to 3, 0 to 0)) { case (newBlock, history) =>
            val (_, restHistory) = WavesChain.dropLiquidBlock(newBlock, history)
            restHistory.head.ref.id shouldBe newBlock.reference
          }
        }
      }
    }

    "dropDifference" - {
      def tests(testGen: Gen[(Vector[WavesBlock], WavesChain, WavesChain)]): Unit = {
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

        "dropped1" - {
          "present in chain1" in forAll(testGen) { case (_, chain1, chain2) =>
            val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
            dropped1.foreach { b1 =>
              withClue(s"b1=$b1, dropped1: $dropped1, dropped2: $dropped2: ") {
                chain1.has(b1.ref) shouldBe true
              }
            }
          }

          "doesn't present in chain2" in forAll(testGen) { case (_, chain1, chain2) =>
            val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
            dropped1.foreach { b1 =>
              withClue(s"b1=$b1, dropped1: $dropped1, dropped2: $dropped2: ") {
                chain2.has(b1.ref) shouldBe false
              }
            }
          }
        }

        "dropped2" - {
          "present in chain2" in forAll(testGen) { case (_, chain1, chain2) =>
            val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
            dropped2.foreach { b2 =>
              withClue(s"b2=$b2, dropped1: $dropped1, dropped2: $dropped2: ") {
                chain2.has(b2.ref) shouldBe true
              }
            }
          }

          "doesn't present in chain1" in forAll(testGen) { case (_, chain1, chain2) =>
            val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
            dropped2.foreach { b2 =>
              withClue(s"b2=$b2, dropped1: $dropped1, dropped2: $dropped2: ") {
                chain1.has(b2.ref) shouldBe false
              }
            }
          }
        }
      }

      "on a fork" - {
        val testGen = for {
          commonBlocks <- historyGen(0 to 2, 0 to 2)
          (maxBlocksNumber, startHeight) = commonBlocks.headOption match {
            case Some(lastBlock) =>
              if (lastBlock.tpe == WavesBlock.Type.FullBlock) (2, lastBlock.ref.height + 1)
              else (0, lastBlock.ref.height)
            case _ => (2, 0)
          }

          detachedHistory1 <- historyGen(0 to maxBlocksNumber, 0 to 2, startHeight to startHeight)
          detachedHistory2 <- historyGen(0 to maxBlocksNumber, 0 to 2, startHeight to startHeight)
        } yield {
          val history1 = detachedHistory1.appendedAll(commonBlocks)
          val history2 = detachedHistory2.appendedAll(commonBlocks)
          (
            commonBlocks,
            WavesChain(history1, history1.headOption.fold(0)(_.ref.height), 100),
            WavesChain(history2, history2.headOption.fold(0)(_.ref.height), 100)
          )
        }

        tests(testGen)
      }

      "on the same chain" - {
        val testGen = for {
          fullHistory <- historyGen(1 to 3, 0 to 2)
          partialHistory <- Gen.choose(1, fullHistory.length).map(fullHistory.drop)
          lessBlockChain <- Gen.oneOf(1, 2)
        } yield {
          val (history1, history2) = if (lessBlockChain == 1) (partialHistory, fullHistory) else (fullHistory, partialHistory)
          (
            partialHistory,
            WavesChain(history1, history1.headOption.fold(0)(_.ref.height), 100),
            WavesChain(history2, history2.headOption.fold(0)(_.ref.height), 100)
          )
        }

        tests(testGen)
      }

      "on totally different chains" - {
        val testGen = for {
          history1 <- historyGen(1 to 3, 0 to 2).map { xs =>
            xs.map { x =>
              x.copy(
                ref = x.ref.copy(id = ByteStr(120 +: x.ref.id.arr)),
                reference = ByteStr(120 +: x.reference.arr)
              )
            }
          }
          history2 <- historyGen(1 to 3, 0 to 2).map { xs =>
            xs.map { x =>
              x.copy(
                ref = x.ref.copy(id = ByteStr(121 +: x.ref.id.arr)),
                reference = ByteStr(121 +: x.reference.arr)
              )
            }
          }
        } yield (mkChain(history1, 100), mkChain(history2, 100))

        "dropped all" in forAll(testGen) { case (chain1, chain2) =>
          val (dropped1, dropped2) = WavesChain.dropDifference(chain1, chain2)
          dropped1 should matchTo(chain1.history.reverse.toList)
          dropped2 should matchTo(chain2.history.reverse.toList)
        }
      }
    }

    "withBlock" - {
      "properties" - {
        val testGen: Gen[(WavesChain, WavesBlock)] =
          Gen.oneOf(
            historyGen(0 to 2, 0 to 2, 0 to 2).map { history =>
              (
                WavesChain(history, history.headOption.fold(0)(_.ref.height), 100),
                mkNextFullBlock(if (history.isEmpty) defaultInitBlock else history.head)
              )
            },
            historyGen(1 to 2, 0 to 2, 0 to 2).map { history =>
              (
                WavesChain(history, history.headOption.fold(0)(_.ref.height), 100),
                mkNextMicroBlock(history.head)
              )
            }
          )

        def test(f: (WavesChain, WavesChain, WavesBlock) => Any): Any = forAll(testGen) { case (chain, newBlock) =>
          chain.withBlock(newBlock) match {
            case Left(e) => fail(e)
            case Right(updatedChain) => f(chain, updatedChain, newBlock)
          }
        }

        "a new block is the last block in the chain after appending" in test { (_, updatedChain, newBlock) =>
          updatedChain.last should matchTo(newBlock.some)
        }

        "the height increased if we append a full block" in test { (chain, updatedChain, newBlock) =>
          val expectedHeight = chain.height + (if (newBlock.tpe == WavesBlock.Type.FullBlock) 1 else 0)
          updatedChain.height shouldBe expectedHeight
        }

        "the capacity decreased if we append a full block" in test { (chain, updatedChain, newBlock) =>
          val expectedCapacity = chain.blocksCapacity - (if (newBlock.tpe == WavesBlock.Type.FullBlock) 1 else 0)
          updatedChain.blocksCapacity shouldBe expectedCapacity
        }

        "the length preserved if we append a block and the capacity exhausted" in {
          val testGen = for {
            history <- historyGen(1 to 2, 0 to 0, 0 to 2)
          } yield (mkChain(history, 0), mkNextFullBlock(history.head))

          forAll(testGen) { case (chain, newBlock) =>
            chain.withBlock(newBlock) match {
              case Left(e) => fail(e)
              case Right(updatedChain) => updatedChain.history.length shouldBe chain.history.length
            }
          }
        }

        "the length increased if we append a micro block even the capacity exhausted" in {
          val testGen = for {
            history <- historyGen(1 to 2, 0 to 2, 0 to 2)
          } yield (mkChain(history, 0), mkNextMicroBlock(history.head))

          forAll(testGen) { case (chain, newBlock) =>
            chain.withBlock(newBlock) match {
              case Left(e) => fail(e)
              case Right(updatedChain) => updatedChain.history.length shouldBe (chain.history.length + 1)
            }
          }
        }
      }

      "empty +" - {
        val init = WavesChain(emptyChain, 0, 100)

        "block" - {
          "valid" in { init.withBlock(block1) should matchTo(mkChain(Vector(block1), 99).asRight[String]) }

          "invalid" in {
            val block1A = block1.copy(ref = block1.ref.copy(height = block1.ref.height + 1))
            init.withBlock(block1A) should matchTo("The new block Ref(h=2, 8TZ) (reference=) must be on height 1".asLeft[WavesChain])
          }
        }

        "micro block" in {
          val microBlock = block1.copy(tpe = WavesBlock.Type.MicroBlock)
          init.withBlock(microBlock) should produce("(?s)^Can't attach a micro block.+to empty chain$".r)
        }
      }

      "block +" - {
        val init = WavesChain(Vector(block1), block1.ref.height, 99)
        "expected" - {
          "block" in { init.withBlock(block2) should matchTo(mkChain(Vector(block2, block1), 98).asRight[String]) }

          "micro block" in {
            val microBlock = block2.copy(
              ref = BlockRef(height = 1, id = block2.ref.id),
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock) should matchTo(mkChain(Vector(microBlock, block1), 99).asRight[String])
          }
        }

        "unexpected" - {
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit = init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must be after.+".r, _.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test("(?s)^The new block.+must be on height.+".r, x => x.copy(ref = x.ref.copy(height = h)))
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
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock,
          confirmedTxs = mkTransactionWithChangesMap(5)
        )

        val init = WavesChain(Vector(microBlock1, block1), microBlock1.ref.height, 99)

        "expected" - {
          "block referenced to the" - {
            "micro block" in {
              val newBlock = WavesBlock(
                ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1))),
                reference = microBlock1.ref.id,
                changes = BlockchainBalance(
                  regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                  outgoingLeasing = Map(alice -> 1L)
                ),
                tpe = WavesBlock.Type.FullBlock,
                confirmedTxs = mkTransactionWithChangesMap(10)
              )

              val hardenedBlock = block1.copy(
                ref = microBlock1.ref,
                reference = block1.reference,
                changes = block1.changes |+| microBlock1.changes,
                tpe = WavesBlock.Type.FullBlock,
                confirmedTxs = block1.confirmedTxs ++ microBlock1.confirmedTxs
              )

              init.withBlock(newBlock) should matchTo(mkChain(Vector(newBlock, hardenedBlock), 98).asRight[String])
            }
          }

          "micro block" in {
            val microBlock2 = block2.copy(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 2))),
              reference = microBlock1.ref.id,
              tpe = WavesBlock.Type.MicroBlock
            )

            init.withBlock(microBlock2) should matchTo(mkChain(Vector(microBlock2, microBlock1, block1), 99).asRight[String])
          }
        }

        "unexpected" - {
          def test(message: Regex, updateNext: WavesBlock => WavesBlock): Unit =
            init.withBlock(updateNext(block2)) should produce(message)

          "block" - {
            "unexpected reference" in test("(?s)^The new block.+must be after.+".r, _.copy(reference = ByteStr.empty))

            "unexpected height" - {
              def heightTest(h: Int): Unit = test("(?s)^The new block.+must be on height.+".r, x => x.copy(ref = x.ref.copy(height = h)))
              "1" in heightTest(1)
              "3" in heightTest(3)
            }

            "referenced to a key block" in {
              val newBlock = WavesBlock(
                ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1))),
                reference = block1.ref.id,
                changes = BlockchainBalance( // TODO changes here are not essential
                  regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                  outgoingLeasing = Map(alice -> 1L)
                ),
                tpe = WavesBlock.Type.FullBlock,
                confirmedTxs = mkTransactionWithChangesMap(10)
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
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock,
          confirmedTxs = mkTransactionWithChangesMap(1)
        )

        val microBlock2 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 2))),
          reference = microBlock1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 3), alice -> Map(usd -> 11)),
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock,
          confirmedTxs = mkTransactionWithChangesMap(2)
        )

        val init = mkChain(Vector(microBlock2, microBlock1, block1), 99)

        "unexpected" - {
          "block referenced to the previous micro block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 2))),
              reference = microBlock1.ref.id,
              changes = BlockchainBalance(
                regular = Map(alice -> Map(Waves -> 9), bob -> Map(usd -> 2L)),
                outgoingLeasing = Map(alice -> 1L)
              ),
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(10)
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
                  outgoingLeasing = Map.empty
                ),
                tpe = WavesBlock.Type.MicroBlock,
                confirmedTxs = mkTransactionWithChangesMap(10)
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
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.MicroBlock,
          confirmedTxs = mkTransactionWithChangesMap(5)
        )

        val init = mkChain(Vector(microBlock, block2, block1), 98)

        val newBlock = WavesBlock(
          ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 1, 0))),
          reference = block1.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 35)),
            outgoingLeasing = Map.empty
          ),
          tpe = WavesBlock.Type.FullBlock,
          confirmedTxs = mkTransactionWithChangesMap(2)
        )

        init.withBlock(newBlock) should produce("(?s)^The new block.+must be after.+".r)
      }
    }

    "dropAfter(ref)" - {
      val testGen = for {
        chain <- defaultChainGen
        refToDrop <- Gen.oneOf(chain.history.map(_.ref))
      } yield (chain, refToDrop)

      "the block with a specified ref" - {
        "remains" in forAll(testGen) { case (chain, ref) =>
          val (updatedChain, _) = chain.dropAfter(ref)
          updatedChain.history.map(_.ref) should contain(ref)
        }

        "should not be among dropped" in forAll(testGen) { case (chain, ref) =>
          val (_, dropped) = chain.dropAfter(ref)
          dropped.map(_.ref) should not contain ref
        }
      }

      "if the ref points to a block with the same or a higher height than of chain" - {
        val testGen = for {
          chain <- defaultChainGen
          refHeight <- Gen.choose(chain.height + 1, chain.height + 3)
        } yield (chain, BlockRef(refHeight, id = ByteStr(Array[Byte](-1, -2, -3))))

        "height invariant is preserved and chain remains" in forAll(testGen) { case (chain, nextRef) =>
          val (updatedChain, _) = chain.dropAfter(nextRef)
          updatedChain should matchTo(chain)
        }

        "no blocks are droppped" in forAll(testGen) { case (chain, nextRef) =>
          val (_, dropped) = chain.dropAfter(nextRef)
          dropped shouldBe empty
        }
      }

      "dropped blocks" - {
        "were in the original chain" in forAll(testGen) { case (chain, ref) =>
          val (_, dropped) = chain.dropAfter(ref)
          dropped.foreach { b =>
            withClue(s"${b.ref}: ") {
              chain.history.map(_.ref) should contain(b.ref)
            }
          }
        }

        "don't present in the updated chain" in forAll(testGen) { case (chain, ref) =>
          val (updatedChain, dropped) = chain.dropAfter(ref)
          dropped.foreach { b =>
            withClue(s"${b.ref}: ") {
              updatedChain.history.map(_.ref) should not contain b.ref
            }
          }
        }

        "are reverse ordered" in forAll(testGen) { case (chain, ref) =>
          val (_, dropped) = chain.dropAfter(ref)
          val droppedRefs = dropped.map(_.ref)
          val expectedRefs = chain.history.take(droppedRefs.length).map(_.ref).reverse

          droppedRefs should contain theSameElementsInOrderAs expectedRefs
        }
      }

      "the chain remains if the we drop after the last block" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          capacity <- Gen.choose(0, 2)
        } yield mkChain(history, capacity)

        forAll(testGen) { chain =>
          val (updatedChain, _) = chain.dropAfter(chain.last.get.ref)
          updatedChain should matchTo(chain)
        }
      }

      "the height of update chain is equal to specified" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          refToDrop <- Gen.oneOf(history.map(_.ref))
          capacity <- Gen.choose(0, 2)
        } yield (mkChain(history, capacity), refToDrop)

        forAll(testGen) { case (chain, ref) =>
          val (updatedChain, _) = chain.dropAfter(ref)
          updatedChain.height shouldBe ref.height
        }
      }

      "the capacity increased by a number of dropped full blocks" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          refToDrop <- Gen.oneOf(history.map(_.ref))
          capacity <- Gen.choose(0, 2)
        } yield (mkChain(history, capacity), refToDrop)

        forAll(testGen) { case (chain, ref) =>
          val (updatedChain, dropped) = chain.dropAfter(ref)
          updatedChain.blocksCapacity shouldBe chain.blocksCapacity + dropped.count(_.tpe == WavesBlock.Type.FullBlock)
        }
      }
    }

    "dropAfter(height)" - {
      val testGen = for {
        history <- historyGen(1 to 3, 0 to 2, 0 to 2)
        refToDrop <- Gen.oneOf(history.map(_.ref))
        capacity <- Gen.choose(0, 2)
      } yield (mkChain(history, capacity), refToDrop.height)

      "the block with a specified height or less" - {
        "remains" in forAll(testGen) { case (chain, height) =>
          val (updatedChain, _) = chain.dropAfter(height)
          updatedChain.history.map(_.ref.height).max shouldBe height
        }

        "should not be among dropped" in forAll(testGen) { case (chain, height) =>
          val (_, dropped) = chain.dropAfter(height)
          dropped.map(_.ref.height).minOption.getOrElse(height) shouldBe >=(height)
        }
      }

      "if the height is higher than of chain" - {
        val testGen = for {
          chain <- defaultChainGen
          dropAfterHeight <- Gen.choose(chain.height + 1, chain.height + 3)
        } yield (chain, dropAfterHeight)

        "height invariant is preserved and chain remains" in forAll(testGen) { case (chain, height) =>
          val (updatedChain, _) = chain.dropAfter(height)
          updatedChain should matchTo(chain)
        }

        "no blocks are droppped" in forAll(testGen) { case (chain, height) =>
          val (_, dropped) = chain.dropAfter(height)
          dropped shouldBe empty
        }
      }

      "dropped blocks" - {
        "were in the original chain" in forAll(testGen) { case (chain, height) =>
          val (_, dropped) = chain.dropAfter(height)
          dropped.foreach { b =>
            withClue(s"${b.ref}: ") {
              chain.history.map(_.ref) should contain(b.ref)
            }
          }
        }

        "don't present in the updated chain" in forAll(testGen) { case (chain, height) =>
          val (updatedChain, dropped) = chain.dropAfter(height)
          dropped.foreach { b =>
            withClue(s"${b.ref}: ") {
              updatedChain.history.map(_.ref.height) should not contain b.ref
            }
          }
        }

        "are with a higher heights the the specified" in forAll(testGen) { case (chain, height) =>
          val (_, dropped) = chain.dropAfter(height)
          dropped.foreach { b =>
            withClue(s"${b.ref}: ") {
              b.ref.height should be > height
            }
          }
        }

        "are reverse ordered" in forAll(testGen) { case (chain, height) =>
          val (_, dropped) = chain.dropAfter(height)
          val droppedRefs = dropped.map(_.ref)
          val expectedRefs = chain.history.take(droppedRefs.length).map(_.ref).reverse

          droppedRefs should contain theSameElementsInOrderAs expectedRefs
        }
      }

      "the chain remains if the we drop after the last block" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          capacity <- Gen.choose(0, 2)
        } yield mkChain(history, capacity)

        forAll(testGen) { chain =>
          val (updatedChain, _) = chain.dropAfter(chain.last.get.ref.height)
          updatedChain should matchTo(chain)
        }
      }

      "the height of update chain is equal to specified" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          refToDrop <- Gen.oneOf(history.map(_.ref))
          capacity <- Gen.choose(0, 2)
        } yield (mkChain(history, capacity), refToDrop.height)

        forAll(testGen) { case (chain, height) =>
          val (updatedChain, _) = chain.dropAfter(height)
          updatedChain.height shouldBe height
        }
      }

      "the capacity increased by a number of dropped full blocks" in {
        val testGen = for {
          history <- historyGen(1 to 3, 0 to 2, 0 to 2)
          refToDrop <- Gen.oneOf(history.map(_.ref))
          capacity <- Gen.choose(0, 2)
        } yield (mkChain(history, capacity), refToDrop.height)

        forAll(testGen) { case (chain, height) =>
          val (updatedChain, dropped) = chain.dropAfter(height)
          updatedChain.blocksCapacity shouldBe chain.blocksCapacity + dropped.count(_.tpe == WavesBlock.Type.FullBlock)
        }
      }
    }

    "withoutLast" - {
      def testGen(maxMicroBlocks: Range = 0 to 2): Gen[WavesChain] = for {
        history <- historyGen(1 to 2, maxMicroBlocks, 0 to 2)
        capacity <- Gen.choose(0, 2)
      } yield mkChain(history, capacity)

      "the last block disappears" in forAll(testGen()) { chain =>
        val (updatedChain, _) = chain.withoutLast
        updatedChain.history should not contain chain.last.get
      }

      "drops the last block" in forAll(testGen()) { chain =>
        val (_, droppedBlock) = chain.withoutLast
        droppedBlock should matchTo(chain.last)
      }

      "the capacity increases if the last block is a full block" in forAll(testGen(0 to 0)) { chain =>
        val (updatedChain, _) = chain.withoutLast
        updatedChain.blocksCapacity shouldBe chain.blocksCapacity + 1
      }

      "the capacity remains if the last block is a micro block" in forAll(testGen(1 to 2)) { chain =>
        val (updatedChain, _) = chain.withoutLast
        updatedChain.blocksCapacity shouldBe chain.blocksCapacity
      }
    }
  }

  private lazy val defaultInitBlock = WavesBlock(
    ref = BlockRef(0, ByteStr(Array[Byte](0))),
    reference = ByteStr.empty,
    changes = Monoid.empty[BlockchainBalance],
    tpe = WavesBlock.Type.FullBlock,
    confirmedTxs = Map.empty
  )

  private def historyGen(blocksNumber: Range, microBlocksNumber: Range, startHeightRange: Range = 0 to 2): Gen[Vector[WavesBlock]] =
    for {
      startHeight <- Gen.choose(startHeightRange.head, startHeightRange.last)
      r <- historyGen(
        blocksNumber,
        microBlocksNumber,
        initBlock = defaultInitBlock.copy(
          ref = BlockRef(
            height = startHeight,
            id = ByteStr(Array.fill[Byte](startHeight)(1))
          )
        )
      )
    } yield r

  private def historyGen(blocksNumber: Range, microBlocksNumber: Range, initBlock: WavesBlock): Gen[Vector[WavesBlock]] = for {
    blocksNumber <- Gen.choose(blocksNumber.head, blocksNumber.last)
    microBlocksNumber <- if (blocksNumber == 0) Gen.const(0) else Gen.choose(microBlocksNumber.head, microBlocksNumber.last)
  } yield historyGen(blocksNumber, microBlocksNumber, initBlock)

  private def historyGen(blocksNumber: Int, microBlocksNumber: Int, initBlock: WavesBlock): Vector[WavesBlock] =
    if (blocksNumber <= 0) Vector.empty
    else {
      val blocks = {
        Iterator(initBlock) ++ Iterator
          .unfold(initBlock) { prev =>
            val next = mkNextFullBlock(prev)
            (next, next).some
          }
      }
        .take(blocksNumber)
        .toVector // 1, 2, 3

      val microBlocks = blocks.lastOption match {
        case None => Vector.empty[WavesBlock]
        case Some(lastBlock) =>
          Iterator
            .unfold(lastBlock) { prev =>
              val next = mkNextMicroBlock(prev)
              (next, next).some
            }
            .take(microBlocksNumber)
            .toVector // 4, 5
      }

      blocks.appendedAll(microBlocks).reverse // 5, 4, 3, 2, 1
    }

  private def mkNextFullBlock(prevBlock: WavesBlock): WavesBlock = prevBlock.copy(
    ref = BlockRef(prevBlock.ref.height + 1, ByteStr(prevBlock.ref.id.arr.prepended(1))),
    reference = prevBlock.ref.id,
    tpe = WavesBlock.Type.FullBlock
  )

  private def mkNextMicroBlock(prevBlock: WavesBlock): WavesBlock = prevBlock.copy(
    ref = prevBlock.ref.copy(id = ByteStr(prevBlock.ref.id.arr.prepended(2))), // height remains
    reference = prevBlock.ref.id,
    tpe = WavesBlock.Type.MicroBlock
  )

}
