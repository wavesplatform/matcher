package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import cats.implicits._
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

import java.nio.charset.StandardCharsets
import scala.collection.immutable.Vector

class StatusTransitionsTestSuite extends WavesIntegrationSuiteBase {

  private val alice = KeyPair(ByteStr("alice".getBytes(StandardCharsets.UTF_8))).toAddress
  private val bob = KeyPair(ByteStr("bob".getBytes(StandardCharsets.UTF_8))).toAddress
  private val carol = KeyPair(ByteStr("caro1".getBytes(StandardCharsets.UTF_8))).toAddress

  private val usd = IssuedAsset(Base58.decode("usd"))

  private val updatedBalances1 = BlockchainBalance(
    regular = Map(alice -> Map(Waves -> 10, usd -> 2L)),
    outgoingLeasing = Map(bob -> 23L)
  )

  private val updatedBalances2 = BlockchainBalance(
    regular = Map(bob -> Map(usd -> 35)),
    outgoingLeasing = Map.empty
  )

  "StatusTransitions" - {
    "Normal +" - {
      "Appended ->" - {
        "Normal" - {
          "a valid block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
              reference = ByteStr.empty,
              changes = updatedBalances1,
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(1)
            )

            val init = Normal(WavesChain(Vector.empty, 0, 100))
            val event = Appended(newBlock)
            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = Normal(mkChain(Vector(newBlock), 99)),
              updatedBalances = updatedBalances1,
              updatedLastBlockHeight = StatusUpdate.LastBlockHeight.Updated(1),
              utxUpdate = UtxUpdate(confirmedTxs = newBlock.confirmedTxs),
              requestNextBlockchainEvent = true
            ))
          }

          "empty chain" - {
            "because of an invalid new block" in test(WavesBlock(
              // height is higher, than in chain
              ref = BlockRef(height = 3, id = ByteStr(Array[Byte](1, 2, 3))),
              reference = ByteStr.empty,
              changes = updatedBalances1,
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(1)
            ))

            "because we try to add a micro block" in test(WavesBlock(
              ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
              reference = ByteStr.empty,
              changes = updatedBalances1,
              tpe = WavesBlock.Type.MicroBlock,
              confirmedTxs = mkTransactionWithChangesMap(1)
            ))

            def test(newBlock: WavesBlock): Unit = {
              val init = Normal(WavesChain(Vector.empty, 1, 100))
              val event = Appended(newBlock)
              StatusTransitions(init, event) should matchTo(StatusUpdate(
                newStatus = init,
                updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired
              ))
            }
          }

          "with RestartRequired because of an invalid new block" in {
            val block1 = WavesBlock(
              ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3))),
              reference = ByteStr.empty,
              changes = updatedBalances1,
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(1)
            )
            val init = Normal(mkChain(Vector(block1), 99))
            val event = Appended(block1)
            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = init,
              updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired
            ))
          }
        }
      }

      "[UtxEvent] -> Normal, where [UtxEvent] is" - {
        "UtxUpdated" in {
          val init = Normal(WavesChain(Vector.empty, 0, 100))
          val newTxs = Seq(UtxTransaction(id = mkTxId(1)))
          val event = UtxUpdated(newTxs, Nil)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init,
            utxUpdate = UtxUpdate(unconfirmedTxs = newTxs),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
          ))
        }

        "UtxSwitched" in {
          val init = Normal(WavesChain(Vector.empty, 0, 100))
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
          confirmedTxs = mkTransactionWithChangesMap(1)
        )

        val block2 = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3, 4))),
          reference = block1.ref.id,
          changes = updatedBalances2,
          tpe = WavesBlock.Type.FullBlock,
          confirmedTxs = mkTransactionWithChangesMap(2)
        )

        val init = Normal(mkChain(Vector(block2, block1), 98))

        def test(to: RolledBack.To): Unit = StatusTransitions(init, RolledBack(to)) should matchTo(StatusUpdate(
          newStatus = TransientRollback(
            fork = WavesFork(init.main, mkChain(Vector(block1), 99)),
            utxUpdate = Monoid.empty[UtxUpdate]
          ),
          requestNextBlockchainEvent = true,
          updatedLastBlockHeight = LastBlockHeight.Updated(1)
        ))

        "Height" in test(RolledBack.To.Height(1))
        "CommonBlockRef" in test(RolledBack.To.CommonBlockRef(block1.ref))
      }

      "DataReceived -> Normal" in {
        val init = Normal(WavesChain(Vector.empty, 0, 100))
        val event = DataReceived(BlockchainBalance(
          regular = Map.empty,
          outgoingLeasing = Map(alice -> 999L)
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
        confirmedTxs = mkTransactionWithChangesMap(1)
      )

      val block2A = WavesBlock(
        ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 0))),
        reference = block1.ref.id,
        changes = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outgoingLeasing = Map.empty
        ),
        tpe = WavesBlock.Type.FullBlock,
        confirmedTxs = mkTransactionWithChangesMap(2)
      )

      val block2B = WavesBlock(
        ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1, 0))),
        reference = block1.ref.id,
        changes = BlockchainBalance(
          regular = Map(bob -> Map(usd -> 12), alice -> Map(usd -> 41)),
          outgoingLeasing = Map.empty
        ),
        tpe = WavesBlock.Type.FullBlock,
        confirmedTxs = mkTransactionWithChangesMap(3)
      )

      val init = TransientRollback(
        fork = WavesFork(
          origChain = mkChain(Vector(block2A, block1), 98),
          forkChain = mkChain(Vector(block2B, block1), 98)
        ),
        utxUpdate = UtxUpdate(
          unconfirmedTxs = List(
            2, // from block2A to UTX Pool during a rollback
            31,
            32
          ).foldMapK(mkUtxTransactionMap).values.toSeq,
          failedTxs = mkUtxTransactionMap(30)
        )
      )

      "Appended ->" - {
        "Normal" in {
          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1, 1))),
            reference = block2B.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 8), carol -> Map(Waves -> 4)),
              outgoingLeasing = Map(bob -> 10)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            confirmedTxs = List(
              2, // The tx migrates from block2A to a new micro block, relates DEX-1099
              10,
              31 // init.utxUpdate
            ).foldMapK(mkTransactionWithChangesMap)
          )

          val event = Appended(microBlock)

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = Normal(
              main = mkChain(Vector(microBlock, block2B, block1), 98)
            ),
            updatedBalances = block2B.changes |+| microBlock.changes,
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.Updated(2),
            utxUpdate = UtxUpdate(
              unconfirmedTxs = mkUtxTransactionMap(32).values.toSeq, // init.utxUpdate, 31 is gone, because confirmed
              confirmedTxs = List(
                3, // block2B
                10, // microBlock
                31 // microBlock
              ).foldMapK(mkTransactionWithChangesMap),
              failedTxs = mkUtxTransactionMap(30) // init.utxUpdate
            ),
            requestNextBlockchainEvent = true
          ))
        }

        "TransientRollback" - {
          "because the resolving process haven't yet completed" in {
            val block3 = WavesBlock(
              ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 2, 0))),
              reference = block2A.ref.id,
              changes = BlockchainBalance(
                regular = Map(carol -> Map(Waves -> 11)),
                outgoingLeasing = Map.empty
              ),
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(10)
            )

            val init = TransientRollback(
              fork = WavesFork(
                origChain = mkChain(Vector(block3, block2A, block1), 97),
                forkChain = mkChain(Vector(block1), 99)
              ),
              utxUpdate = UtxUpdate(confirmedTxs = mkTransactionWithChangesMap(1))
            )

            val event = Appended(block2B)

            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = TransientRollback(
                fork = WavesFork(init.fork.origChain, mkChain(Vector(block2B, block1), 98)),
                utxUpdate = init.utxUpdate
              ),
              requestNextBlockchainEvent = true,
              updatedLastBlockHeight = LastBlockHeight.Updated(2)
            ))
          }

          "because of an unrelated new block" in {
            val newBlock = WavesBlock(
              ref = BlockRef(height = 3, id = ByteStr(Array[Byte](98, 2, 0))),
              reference = ByteStr(Array[Byte](98, 2, -1)),
              changes = BlockchainBalance(
                regular = Map(alice -> Map(usd -> 8), carol -> Map(Waves -> 4)),
                outgoingLeasing = Map(bob -> 10)
              ),
              tpe = WavesBlock.Type.FullBlock,
              confirmedTxs = mkTransactionWithChangesMap(10)
            )

            val event = Appended(newBlock)

            StatusTransitions(init, event) should matchTo(StatusUpdate(
              newStatus = init,
              updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired
            ))
          }
        }

        "TransientResolving" in {
          val microBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](98, 1, 1))),
            reference = block2B.ref.id,
            changes = BlockchainBalance(
              regular = Map(alice -> Map(usd -> 8)),
              outgoingLeasing = Map(bob -> 10)
            ),
            tpe = WavesBlock.Type.MicroBlock,
            confirmedTxs = mkTransactionWithChangesMap(10)
          )

          val event = Appended(microBlock)

          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = TransientResolving(
              main = mkChain(Vector(microBlock, block2B, block1), 98),
              stashChanges = BlockchainBalance( // block2B + microBlock
                regular = Map(alice -> Map(usd -> 8), bob -> Map(usd -> 12)),
                outgoingLeasing = Map(bob -> 10)
              ),
              utxUpdate = init.utxUpdate |+| UtxUpdate(
                confirmedTxs = block2B.confirmedTxs ++ microBlock.confirmedTxs,
                failedTxs = Map.empty // Doesn't affect
              )
            ),
            requestBalances = DiffIndex(regular = Map(carol -> Set(Waves: Asset)), outgoingLeasing = Set.empty),
            updatedLastBlockHeight = LastBlockHeight.Updated(microBlock.ref.height)
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
                origChain = mkChain(Vector(block2A, block1), 98),
                forkChain = mkChain(Vector(block1), 99)
              ),
              utxUpdate = init.utxUpdate
            ),
            requestNextBlockchainEvent = true,
            updatedLastBlockHeight = LastBlockHeight.Updated(1)
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
        confirmedTxs = mkTransactionWithChangesMap(1)
      )

      val microBlock = WavesBlock(
        ref = BlockRef(height = 1, id = ByteStr(Array[Byte](98, 1))),
        reference = block.ref.id,
        changes = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outgoingLeasing = Map.empty
        ),
        tpe = WavesBlock.Type.MicroBlock,
        confirmedTxs = mkTransactionWithChangesMap(2)
      )

      val init = TransientResolving(
        main = mkChain(Vector(microBlock, block), 99),
        stashChanges = BlockchainBalance(
          regular = Map(carol -> Map(Waves -> 10)),
          outgoingLeasing = Map.empty
        ),
        utxUpdate = UtxUpdate(failedTxs = mkUtxTransactionMap(30))
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
            outgoingLeasing = Map(carol -> 10)
          ),
          tpe = WavesBlock.Type.MicroBlock,
          confirmedTxs = mkTransactionWithChangesMap(10)
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
                failedTxs = event.failedTxs.headOption.map(x => x.id -> x).toMap
              )
            )
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
          outgoingLeasing = Map.empty
        ))
        StatusTransitions(init, event) should matchTo(StatusUpdate(
          newStatus = Normal(init.main),
          updatedBalances = BlockchainBalance( // stashChanges + event
            regular = Map(
              alice -> Map(Waves -> 1),
              carol -> Map(Waves -> 10)
            ),
            outgoingLeasing = Map.empty
          ),
          updatedLastBlockHeight = LastBlockHeight.Updated(init.main.height),
          utxUpdate = init.utxUpdate,
          requestNextBlockchainEvent = true
        ))
      }
    }
  }

}
