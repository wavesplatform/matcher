package com.wavesplatform.dex.domain.feature

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val SmartAssets: BlockchainFeature = BlockchainFeature(9, "Smart Assets")
  val SmartAccountTrading: BlockchainFeature = BlockchainFeature(10, "Smart Account Trading")
  val OrderV3: BlockchainFeature = BlockchainFeature(12, "Order Version 3")
  val RideV6: BlockchainFeature = BlockchainFeature(17, "Ride V6, MetaMask support")
}
