package com.wavesplatform.dex.grpc.integration.clients.domain

import java.nio.charset.StandardCharsets

import cats.Monoid
import cats.syntax.semigroup._
import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.clients.domain.BlockchainStatus.{Normal, TransientResolving, TransientRollback}
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent.{Appended, DataReceived, RolledBack, UtxSwitched, UtxUpdated}
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

import scala.collection.immutable.Vector

class StatusTransitionsTestSuite extends WavesIntegrationSuiteBase {

  private val alice = KeyPair(ByteStr("alice".getBytes(StandardCharsets.UTF_8))).toAddress
  private val bob = KeyPair(ByteStr("bob".getBytes(StandardCharsets.UTF_8))).toAddress
  private val carol = KeyPair(ByteStr("caro1".getBytes(StandardCharsets.UTF_8))).toAddress

  private val usd = IssuedAsset(Base58.decode("usd"))

  private val updatedBalances1 = BlockchainBalance(
    regular = Map(alice -> Map(Waves -> 10, usd -> 2L)),
    outLeases = Map(bob -> 23L)
  )

  private val updatedBalances2 = BlockchainBalance(
    regular = Map(bob -> Map(usd -> 35)),
    outLeases = Map.empty
  )

  "StatusTransitions" - {
    "Normal +" - {
      "Appended ->" - {
        "Normal" in {
          val newBlock = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
            reference = ByteStr.empty,
            changes = updatedBalances1,
            tpe = WavesBlock.Type.FullBlock,
            forgedTxs = Set(mkTxId(1))
          )

          val init = Normal(WavesChain(Vector.empty, 100))
          val event = Appended(newBlock)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = Normal(WavesChain(Vector(newBlock), 99)),
            updatedBalances = updatedBalances1,
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.Updated(1),
            utxUpdate = UtxUpdate(forgedTxs = newBlock.forgedTxs),
            requestNextBlockchainEvent = true
          ))
        }

        "TransientRollback" - {
          "because of an invalid new block" in {
            val block1 = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3))),
              reference = ByteStr.empty,
              changes = updatedBalances1,
              tpe = WavesBlock.Type.FullBlock,
              forgedTxs = Set(mkTxId(1))
            )
            val init = Normal(WavesChain(Vector(block1), 99))
            val event = Appended(block1)
            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = TransientRollback(
                fork = WavesFork(init.main, WavesChain(Vector.empty, 1, 100)),
                utxUpdate = Monoid.empty[UtxUpdate]
              ),
              updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired(2)
            ))
          }
        }
      }

      "[UtxEvent] -> Normal, where [UtxEvent] is" - {
        "UtxUpdated" in {
          val init = Normal(WavesChain(Vector.empty, 100))
          val newTxs = Seq(UtxTransaction(id = mkTxId(1)))
          val event = UtxUpdated(newTxs, Nil)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init,
            utxUpdate = UtxUpdate(unconfirmedTxs = newTxs),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
          ))
        }

        "UtxSwitched" in {
          val init = Normal(WavesChain(Vector.empty, 100))
          val txs = Seq(UtxTransaction(id = mkTxId(1)))
          val event = UtxSwitched(txs)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init,
            utxUpdate = UtxUpdate(unconfirmedTxs = txs, resetCaches = true),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
          ))
        }
      }

      "[RolledBack] -> Normal, where [RolledBack] is" - {
        val block1 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
          reference = ByteStr.empty,
          changes = updatedBalances1,
          tpe = WavesBlock.Type.FullBlock,
          forgedTxs = Set(mkTxId(1))
        )

        val block2 = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3, 4))),
          reference = block1.ref.id,
          changes = updatedBalances2,
          tpe = WavesBlock.Type.FullBlock,
          forgedTxs = Set(mkTxId(2))
        )

        val init = Normal(WavesChain(Vector(block2, block1), 98))

        def test(to: RolledBack.To): Unit = StatusTransitions(init, RolledBack(to)) should matchTo(StatusUpdate(
          newStatus = TransientRollback(
            fork = WavesFork(init.main, WavesChain(Vector(block1), 99)),
            utxUpdate = Monoid.empty[UtxUpdate]
          ),
          updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged,
          requestNextBlockchainEvent = true
        ))

        "Height" in test(RolledBack.To.Height(1))
        "CommonBlockRef" in test(RolledBack.To.CommonBlockRef(block1.ref))
      }

      "DataReceived -> Normal" in {
        val init = Normal(WavesChain(Vector.empty, 100))
        val event = DataReceived(BlockchainBalance(
          regular = Map.empty,
          outLeases = Map(alice -> 999L)
        ))
        StatusTransitions(init, event) should matchTo(StatusUpdate(
          newStatus = init, // Ignoring
          requestNextBlockchainEvent = true
        ))
      }
    }

    "TransientRollback +" - {
      val block1 = WavesBlock(
        ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0, 0))),
        reference = ByteStr.empty,
        changes = updatedBalances1,
        tpe = WavesBlock.Type.FullBlock,
        forgedTxs = Set(mkTxId(1))
      )

      val block2A = WavesBlock(
        ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
        reference = block1.ref.id,
        changes = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outLeases = Map.empty
        ),
        tpe = WavesBlock.Type.FullBlock,
        forgedTxs = Set(mkTxId(2))
      )

      val block2B = WavesBlock(
        ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1, 0))),
        reference = block1.ref.id,
        changes = BlockchainBalance(
          regular = Map(bob -> Map(usd -> 12), alice -> Map(usd -> 41)),
          outLeases = Map.empty
        ),
        tpe = WavesBlock.Type.FullBlock,
        forgedTxs = Set(mkTxId(3))
      )

      val init = TransientRollback(
        fork = WavesFork(
          origChain = WavesChain(Vector(block2A, block1), 98),
          forkChain = WavesChain(Vector(block2B, block1), 98)
        ),
        utxUpdate = UtxUpdate(failedTxs = Set(mkTxId(30)))
      )

      "Appended ->" - {
        "Normal" in {
          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1, 1))),
            reference = block2B.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 8), carol -> Map(Waves -> 4)),
              outLeases = Map(bob -> 10)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            forgedTxs = Set(mkTxId(10))
          )

          val event = Appended(microBlock)

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = Normal(
              main = WavesChain(Vector(microBlock, block2B, block1), 98)
            ),
            updatedBalances = block2B.changes |+| microBlock.changes,
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.Updated(2),
            utxUpdate = init.utxUpdate |+| UtxUpdate(
              forgedTxs = block2B.forgedTxs.union(microBlock.forgedTxs),
              failedTxs = block2A.forgedTxs // It doesn't affect now
            ),
            requestNextBlockchainEvent = true
          ))
        }

        "TransientRollback" - {
          "because of an unrelated new block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 2, 0))),
              reference = ByteStr(Array[Byte](98, 2, -1)),
              changes = BlockchainBalance(
                regular = Map(alice -> Map(usd -> 8), carol -> Map(Waves -> 4)),
                outLeases = Map(bob -> 10)
              ),
              tpe = WavesBlock.Type.FullBlock,
              forgedTxs = Set(mkTxId(10))
            )

            val event = Appended(newBlock)

            // See StatusTransitions
            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = TransientRollback(
                // TODO test with more than 1 block
                fork = WavesFork(init.fork.origChain, WavesChain(Vector(block1), 99)),
                utxUpdate = init.utxUpdate
              ),
              updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired(2)
            ))
          }

          "because the resolving process haven't yet completed" in {
            val block3 = WavesBlock(
              ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 2, 0))),
              reference = block2A.ref.id,
              changes = BlockchainBalance(
                regular = Map(carol -> Map(Waves -> 11)),
                outLeases = Map.empty
              ),
              tpe = WavesBlock.Type.FullBlock,
              forgedTxs = Set(mkTxId(10))
            )

            val init = TransientRollback(
              fork = WavesFork(
                origChain = WavesChain(Vector(block3, block2A, block1), 97),
                forkChain = WavesChain(Vector(block1), 99)
              ),
              utxUpdate = UtxUpdate(forgedTxs = Set(mkTxId(1)))
            )

            val event = Appended(block2B)

            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = TransientRollback(
                fork = WavesFork(init.fork.origChain, WavesChain(Vector(block2B, block1), 98)),
                utxUpdate = init.utxUpdate
              ),
              requestNextBlockchainEvent = true
            ))
          }
        }

        "TransientResolving" in {
          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
            reference = block2B.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 8)),
              outLeases = Map(bob -> 10)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            forgedTxs = Set(mkTxId(10))
          )

          val event = Appended(microBlock)

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = TransientResolving(
              main = WavesChain(Vector(microBlock, block2B, block1), 98),
              stashChanges = BlockchainBalance( // block2B + microBlock
                regular = Map(alice -> Map(usd -> 8), bob -> Map(usd -> 12)),
                outLeases = Map(bob -> 10)
              ),
              utxUpdate = init.utxUpdate |+| UtxUpdate(
                forgedTxs = block2B.forgedTxs.union(microBlock.forgedTxs),
                failedTxs = block2A.forgedTxs // Doesn't affect
              )
            ),
            requestBalances = DiffIndex(regular = Map(carol -> Set(Waves: Asset)), outLeases = Set.empty),
            updatedLastBlockHeight = LastBlockHeight.NotChanged
          ))
        }
      }

      "[UtxEvent] -> TransientRollback, where [UtxEvent] is" - {
        "UtxUpdated" in {
          val newTxs = Seq(UtxTransaction(id = UnsafeByteOperations.unsafeWrap(Array[Byte](5, 6, 7))))
          val event = UtxUpdated(newTxs, Nil)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init.copy(
              utxUpdate = init.utxUpdate |+| UtxUpdate(unconfirmedTxs = newTxs)
            )
          ))
        }

        "UtxSwitched" in {
          val txs = Seq(UtxTransaction(id = UnsafeByteOperations.unsafeWrap(Array[Byte](5, 6, 7))))
          val event = UtxSwitched(txs)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init.copy(
              utxUpdate = UtxUpdate(
                unconfirmedTxs = txs,
                resetCaches = true
              )
            )
          ))
        }
      }

      "[RolledBack] -> TransientRollback, where [RolledBack] is" - {
        def test(rolledBackTo: RolledBack.To): Unit =
          StatusTransitions(init, RolledBack(rolledBackTo)) should matchTo(StatusUpdate(
            newStatus = TransientRollback(
              fork = WavesFork(
                origChain = WavesChain(Vector(block2A, block1), 98),
                forkChain = WavesChain(Vector(block1), 99)
              ),
              utxUpdate = init.utxUpdate
            ),
            requestNextBlockchainEvent = true
          ))

        "Height" in test(RolledBack.To.Height(1))
        "CommonBlockRef" in test(RolledBack.To.CommonBlockRef(block1.ref))
      }

      "DataReceived -> TransientRollback" in {
        StatusTransitions(init, DataReceived(Monoid.empty[BlockchainBalance])) should matchTo(StatusUpdate(
          newStatus = init, // ignored
          requestNextBlockchainEvent = true
        ))
      }
    }

    "TransientResolving +" - {
      val block = WavesBlock(
        ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 0))),
        reference = ByteStr.empty,
        changes = updatedBalances1,
        tpe = WavesBlock.Type.FullBlock,
        forgedTxs = Set(mkTxId(1))
      )

      val microBlock = WavesBlock(
        ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 1))),
        reference = block.ref.id,
        changes = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outLeases = Map.empty
        ),
        tpe = WavesBlock.Type.MicroBlock,
        forgedTxs = Set(mkTxId(2))
      )

      val init = TransientResolving(
        main = WavesChain(Vector(microBlock, block), 99),
        stashChanges = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outLeases = Map.empty
        ),
        utxUpdate = UtxUpdate(failedTxs = Set(mkTxId(30)))
      )

      def stashedTest(event: WavesNodeEvent, requestNextBlockchainEvent: Boolean): Unit =
        StatusTransitions(init, event) should matchTo(StatusUpdate(
          newStatus = init,
          requestNextBlockchainEvent = requestNextBlockchainEvent
        ))

      "Appended -> TransientResolving" in {
        val microBlock2 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 2))),
          reference = microBlock.ref.id,
          changes = BlockchainBalance(
            regular = Map(bob -> Map(usd -> 1)),
            outLeases = Map(carol -> 10)
          ),
          tpe = WavesBlock.Type.MicroBlock,
          forgedTxs = Set(mkTxId(10))
        )

        stashedTest(Appended(microBlock2), requestNextBlockchainEvent = true)
      }

      "[UtxEvent] -> TransientResolving, where [UtxEvent] is" - {
        "UtxUpdated" in {
          val event = UtxUpdated(
            addedTxs = Seq(UtxTransaction(id = mkTxId(10))),
            failedTxs = Seq(UtxTransaction(id = mkTxId(11)))
          )

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init.copy(
              utxUpdate = init.utxUpdate |+| UtxUpdate(
                unconfirmedTxs = event.addedTxs,
                failedTxs = event.failedTxs.headOption.map(_.id).toSet
              )
            ),
          ))
        }

        "UtxSwitched" in {
          val event = UtxSwitched(Seq(UtxTransaction(id = mkTxId(10))))

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init.copy(
              utxUpdate = UtxUpdate(unconfirmedTxs = event.newTxs, resetCaches = true)
            )
          ))
        }
      }

      "[RolledBack] -> TransientResolving, where [RolledBack] is" - {
        "Height" in stashedTest(RolledBack(RolledBack.To.Height(1)), requestNextBlockchainEvent = true)
        "CommonBlockRef" in stashedTest(RolledBack(RolledBack.To.CommonBlockRef(block.ref)), requestNextBlockchainEvent = true)
      }

      // TODO more tests
      "DataReceived -> Normal" in {
        val event = DataReceived(BlockchainBalance(
          regular = Map(alice -> Map(Waves -> 1)),
          outLeases = Map.empty
        ))
        StatusTransitions(init, event) should matchTo(StatusUpdate(
          newStatus = Normal(init.main),
          updatedBalances = BlockchainBalance( // stashChanges + event
            regular = Map(
              alice -> Map(Waves -> 1),
              carol -> Map(Waves -> 10)
            ),
            outLeases = Map.empty
          ),
          updatedLastBlockHeight = LastBlockHeight.Updated(init.main.height),
          utxUpdate = init.utxUpdate,
          requestNextBlockchainEvent = true
        ))
      }
    }
  }

}
