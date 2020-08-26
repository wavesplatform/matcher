package com.wavesplatform.dex.api.http.routes

import akka.actor._
import akka.cluster.sharding.{ClusterSharding, ShardRegion}
import akka.pattern.ask
import akka.util.Timeout
import com.wavesplatform.dex.actors.address.AddressDirectoryActor

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class ApiService(system: ActorSystem, addressDirectoryRef: ActorRef) {
  private implicit val timeout = Timeout(10.seconds)
  private val sharding         = ClusterSharding(system)

  private val ref = sharding.start(
    typeName = "AddressDirectoryRef",
    entityProps = ApiService.props(addressDirectoryRef),
    extractEntityId = {
      case x: AddressDirectoryActor.Envelope => (x.address.toString, x)
    }: ShardRegion.ExtractEntityId,
    extractShardId = {
      case x: AddressDirectoryActor.Envelope => (x.address.bytes.hashCode % 10).toString
    }: ShardRegion.ExtractShardId
  )

  def askMessage(message: AddressDirectoryActor.Envelope): Future[Any] = ref.ask(message)
}

object ApiService {
  class AddressActorReferenceActor(addressDirectoryRef: ActorRef) extends Actor with ActorLogging {
    override def receive: Receive = {
      case x => addressDirectoryRef.forward(x)
    }
  }

  def props(addressDirectoryRef: ActorRef): Props = Props(new AddressActorReferenceActor(addressDirectoryRef))
}
