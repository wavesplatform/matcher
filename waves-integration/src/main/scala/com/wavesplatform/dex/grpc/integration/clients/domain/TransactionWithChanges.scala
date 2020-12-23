package com.wavesplatform.dex.grpc.integration.clients.domain

import com.google.protobuf.ByteString
import com.wavesplatform.events.protobuf.StateUpdate
import com.wavesplatform.protobuf.transaction.SignedTransaction

/**
 * @param txId Here only for tests to fix "Class to large" compiler issue
 */
case class TransactionWithChanges(txId: ByteString, tx: SignedTransaction, changes: StateUpdate)
