sealed abstract class NodeNetwork(val suffix: String) {
  lazy val packageSuffix = if (suffix == Mainnet.suffix) "" else "-" + suffix
  override val toString = suffix
}

object NodeNetwork {
  def apply(v: Option[String]) = v match {
    case Some(Testnet.suffix) => Testnet
    case Some(Devnet.suffix) => Devnet
    case _ => Mainnet
  }
  
  val All: List[NodeNetwork] = List(Mainnet, Testnet, Devnet)
}

object Mainnet extends NodeNetwork("mainnet")
object Testnet extends NodeNetwork("testnet")
object Devnet extends NodeNetwork("devnet")
