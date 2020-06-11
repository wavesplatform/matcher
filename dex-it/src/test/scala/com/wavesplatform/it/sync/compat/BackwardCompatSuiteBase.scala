package com.wavesplatform.it.sync.compat

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.it.api.MultipleVersions
import com.wavesplatform.dex.it.dex.DexApi
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.MatcherState

/**
  * Doesn't start DEX in beforeAll
  */
trait BackwardCompatSuiteBase extends MatcherSuiteBase with MultipleVersions {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  protected val carol    = mkKeyPair("carol")
  protected val accounts = List(alice, bob)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)
    broadcastAndAwait(
      IssueUsdTx,
      IssueEthTx,
      mkTransfer(alice, carol, 1.003.waves, Waves)
    )
    wavesNode1.api.waitForHeightArise()
    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)
    broadcastAndAwait(
      mkTransfer(alice, bob, IssueUsdTx.getQuantity / 2, usd),
      mkTransfer(alice, bob, IssueEthTx.getQuantity / 2, eth)
    )
  }

  protected def waitOnBoth(order: Order, status: Status): Unit = {
    dex1.api.waitForOrderStatus(order, status)
    dex2.api.waitForOrderStatus(order, status)
  }

  protected def cancelAll(): Unit = {
    accounts.foreach(dex2.api.cancelAll(_))
    accounts.foreach(dex2.api.waitForOrderHistory(_, activeOnly = Some(true))(_.isEmpty))
  }

  protected def state(dexApi: DexApi[Id], orders: IndexedSeq[Order]): MatcherState = clean(matcherState(List(wavesUsdPair), orders, accounts, dexApi))

  private def clean(state: MatcherState): MatcherState = state.copy(
    offset = 0L, // doesn't matter in this test
    // we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )

}
