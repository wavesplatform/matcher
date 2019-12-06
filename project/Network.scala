sealed abstract class NodeNetwork(val suffix: String) {
  lazy val packageSuffix = if (suffix == Mainnet.suffix) "" else "-" + suffix
  override val toString  = suffix
}

object NodeNetwork {
  def apply(v: Option[String]): NodeNetwork = v match {
    case Some(Testnet.suffix)  => Testnet
    case Some(Stagenet.suffix) => Stagenet
    case Some(Devnet.suffix)   => Devnet
    case _                     => Mainnet
  }

  val All: List[NodeNetwork] = List(Mainnet, Testnet, Stagenet, Devnet)
}

object Mainnet  extends NodeNetwork("mainnet")
object Testnet  extends NodeNetwork("testnet")
object Stagenet extends NodeNetwork("stagenet")
object Devnet   extends NodeNetwork("devnet")
