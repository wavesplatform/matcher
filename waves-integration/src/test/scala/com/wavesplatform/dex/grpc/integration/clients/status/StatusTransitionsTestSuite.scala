package com.wavesplatform.dex.grpc.integration.clients.status

import java.nio.charset.StandardCharsets

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.dex.WavesIntegrationSuiteBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.grpc.integration.clients.status.BlockchainStatus.{Normal, TransientRollback}
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent.{Appended, RolledBack, UtxAdded, UtxSwitched, WavesNodeUtxEvent}
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction

import scala.collection.immutable.Queue

class StatusTransitionsTestSuite extends WavesIntegrationSuiteBase {

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

  private val updatedBalances = updatedBalances1 |+| updatedBalances2

  "StatusTransitions" - {
    "Normal +" - {
      "Appended ->" - {
        "Normal" in {
          val newBlock = WavesBlock(
            ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
            reference = ByteStr.empty,
            changes = updatedBalances,
            tpe = WavesBlock.Type.Block
          )

          val init = Normal(WavesFork(List.empty), 0)
          val event = Appended(block = newBlock, forgedTxIds = Seq.empty)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = Normal(
              mainFork = WavesFork(List(newBlock)),
              currentHeightHint = 1
            ),
            updatedBalances = updatedBalances,
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.Updated(1)
          ))
        }

        "TransientRollback" in {
          val prevBlock = WavesBlock(
            ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3))),
            reference = ByteStr.empty,
            changes = updatedBalances,
            tpe = WavesBlock.Type.Block
          )
          val init = Normal(WavesFork(List(prevBlock)), prevBlock.ref.height)
          val event = Appended(block = prevBlock, forgedTxIds = Seq.empty)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = TransientRollback(
              newFork = WavesFork(List.empty),
              newForkChanges = Monoid.empty[BlockchainBalance],
              previousForkHeight = 2,
              previousForkDiffIndex = updatedBalances.diffIndex,
              utxEventsStash = Queue.empty
            ),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.RestartRequired(1)
          ))
        }
      }

      "[UtxEvent] -> TransientRollback" - {
        "UtxAdded" in {
          val init = Normal(WavesFork(List.empty), 0)
          val txs = Seq(UtxTransaction(id = UnsafeByteOperations.unsafeWrap(Array[Byte](5, 6, 7))))
          val event = UtxAdded(txs)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init,
            processUtxEvents = Queue(WavesNodeUtxEvent.Added(txs)),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
          ))
        }

        "UtxSwitched" in {
          val init = Normal(WavesFork(List.empty), 0)
          val txs = Seq(UtxTransaction(id = UnsafeByteOperations.unsafeWrap(Array[Byte](5, 6, 7))))
          val event = UtxSwitched(txs)
          StatusTransitions(init, event) should matchTo(StatusUpdate(
            newStatus = init,
            processUtxEvents = Queue(WavesNodeUtxEvent.Switched(txs)),
            updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
          ))
        }
      }

      "[RolledBack] -> Normal" - {
        val block1 = WavesBlock(
          ref = BlockRef(height = 1, id = ByteStr(Array[Byte](1, 2, 3))),
          reference = ByteStr.empty,
          changes = updatedBalances1,
          tpe = WavesBlock.Type.Block
        )

        val block2 = WavesBlock(
          ref = BlockRef(height = 2, id = ByteStr(Array[Byte](1, 2, 3, 4))),
          reference = block1.ref.id,
          changes = updatedBalances2,
          tpe = WavesBlock.Type.Block
        )

        val init = Normal(WavesFork(List(block2, block1)), 2)

        def test(to: RolledBack.To): Unit = StatusTransitions(init, RolledBack(to)) should matchTo(StatusUpdate(
          newStatus = TransientRollback(
            newFork = WavesFork(List(block1)),
            newForkChanges = Monoid.empty[BlockchainBalance],
            previousForkHeight = 2,
            previousForkDiffIndex = updatedBalances2.diffIndex,
            utxEventsStash = Queue.empty
          ),
          updatedLastBlockHeight = StatusUpdate.LastBlockHeight.NotChanged
        ))

        "Height" in test(RolledBack.To.Height(1))
        "CommonBlockRef" in test(RolledBack.To.CommonBlockRef(block1.ref))
      }
    }
  }

}
