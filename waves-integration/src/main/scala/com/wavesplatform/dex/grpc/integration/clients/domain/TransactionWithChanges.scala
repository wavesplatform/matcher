package com.wavesplatform.dex.grpc.integration.clients.domain

import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.SignedTransaction

case class TransactionWithChanges(tx: SignedTransaction, changes: StateUpdate)
