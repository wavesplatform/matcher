package com.wavesplatform.dex.grpc.integration.clients.domain.portfolio

import com.google.protobuf.ByteString

case class PessimisticTransaction(txId: ByteString, pessimisticPortfolio: AddressAssets)
