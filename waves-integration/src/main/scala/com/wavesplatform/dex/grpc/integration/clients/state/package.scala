package com.wavesplatform.dex.grpc.integration.clients

import supertagged._

package object state {
  object WavesBlock extends TaggedType[BaseBlock]
  type WavesBlock = WavesBlock.Type

  object WavesMicroBlock extends TaggedType[BaseBlock]
  type WavesMicroBlock = WavesMicroBlock.Type
}
