//package com.wavesplatform.dex.actors.tx
//
//import akka.actor.typed
//import akka.actor.typed.Behavior
//import akka.actor.typed.scaladsl.Behaviors
//import com.wavesplatform.dex.actors.OrderEventsCoordinatorActor
//import com.wavesplatform.dex.domain.bytes.ByteStr
//import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
//
//import scala.concurrent.Future
//import scala.util.{Success, Try}
//
//object NewBroadcastExchangeTransactionActor {
//  type ClientRef = typed.ActorRef[OrderEventsCoordinatorActor.Message]
//
//  sealed trait Message extends Product with Serializable
//
//  sealed trait Command extends Message
//
//  object Command {
//    // TODO Message
//    case class Broadcast(clientRef: ClientRef, tx: ExchangeTransaction) extends Command
//  }
//
//  sealed trait Event extends Message
//
//  object Event {
//    case class TransactionChecked(clientRef: ClientRef, tx: ExchangeTransaction, isKnown: Try[Boolean]) extends Event
//  }
//
//  // TODO confirmed AND appeared
//  def apply(
//    isTransactionKnown: ExchangeTransaction.Id => Future[Boolean],
//    confirmed: Seq[ByteStr] => Future[Map[ByteStr, Boolean]],
//    broadcast: ExchangeTransaction => Future[Boolean]
//  ): Behavior[Message] = Behaviors.setup { _ =>
//    def default(): Behavior[Message] = Behaviors.receive[Message] { (context, message) =>
//      message match {
//        case Command.Broadcast(clientRef, tx) =>
//          context.pipeToSelf(isTransactionKnown(tx.id()))(Event.TransactionChecked(clientRef, tx, _))
//          Behaviors.same
//
//        case Event.TransactionChecked(clientRef, tx, isKnown) =>
//          isKnown match {
//            case Success(false) => broadcast(tx) // TODO
//            case _ => clientRef ! OrderEventsCoordinatorActor.Command.ApplyObserved(tx) // TODO better logic for Failure
//          }
//          Behaviors.same
//      }
//    }
//
//    default()
//  }
//
//}
