package com.wavesplatform.dex.api

import java.util.concurrent.atomic.AtomicReference

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.grpc.GRPCErrors
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.{
  AssetAmount,
  BurnTransactionData,
  CreateAliasTransactionData,
  ExchangeTransactionData,
  GenesisTransactionData,
  InvokeScriptTransactionData,
  IssueTransactionData,
  LeaseCancelTransactionData,
  LeaseTransactionData,
  PBAmounts,
  PBOrders,
  PBRecipients,
  PBSignedTransaction,
  PBTransaction,
  PBTransactions,
  PaymentTransactionData,
  Recipient,
  ReissueTransactionData,
  SetAssetScriptTransactionData,
  SetScriptTransactionData,
  SponsorFeeTransactionData,
  TransferTransactionData,
  VanillaAssetId,
  VanillaTransaction
}
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.transaction.PBTransactions.{createVanilla, createVanillaUnsafe, toVanillaDataEntry}
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.{Asset, Proofs, TxValidationError}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer

import scala.concurrent.Future

package object grpc {
  implicit class StreamObserverMonixOps[T](streamObserver: StreamObserver[T])(implicit sc: Scheduler) {
    // TODO: More convenient back-pressure implementation
    def toSubscriber: monix.reactive.observers.Subscriber[T] = {
      import org.reactivestreams.{Subscriber, Subscription}

      val rxs = new Subscriber[T] with Cancelable {
        private[this] val element = new AtomicReference[Option[T]](None)

        @volatile
        private[this] var subscription: Subscription = _

        private[this] val observerReadyFunc: () => Boolean = streamObserver match {
          case callStreamObserver: CallStreamObserver[_] =>
            () =>
              callStreamObserver.isReady
          case _ =>
            () =>
              true
        }

        def isReady: Boolean = observerReadyFunc()

        override def onSubscribe(subscription: Subscription): Unit = {
          this.subscription = subscription

          def pushElement(): Unit = element.get() match {
            case v @ Some(value) if this.isReady =>
              if (element.compareAndSet(v, None)) {
                streamObserver.onNext(value)
                subscription.request(1)
              } else {
                pushElement()
              }

            case None if this.isReady =>
              subscription.request(1)

            case _ =>
            // Ignore
          }

          subscription match {
            case scso: ServerCallStreamObserver[T] =>
              scso.disableAutoInboundFlowControl()
              scso.setOnCancelHandler(() => subscription.cancel())
              scso.setOnReadyHandler(() => pushElement())
            // subscription.request(1)

            case cso: CallStreamObserver[T] =>
              cso.disableAutoInboundFlowControl()
              cso.setOnReadyHandler(() => pushElement())
            // subscription.request(1)

            case _ =>
              subscription.request(Long.MaxValue)
          }
        }

        override def onNext(t: T): Unit = {
          if (isReady) {
            val value = element.get()
            if (value.nonEmpty) {
              if (element.compareAndSet(value, Some(t))) streamObserver.onNext(value.get)
              else onNext(t)
            } else {
              streamObserver.onNext(t)
            }
            if (isReady) subscription.request(1)
          } else if (!element.compareAndSet(None, Some(t))) {
            throw new IllegalArgumentException("Buffer overflow")
          }
        }

        override def onError(t: Throwable): Unit = streamObserver.onError(GRPCErrors.toStatusException(t))
        override def onComplete(): Unit          = streamObserver.onCompleted()
        def cancel(): Unit                       = Option(subscription).foreach(_.cancel())
      }

      monix.reactive.observers.Subscriber.fromReactiveSubscriber(rxs, rxs)
    }

    def completeWith(obs: Observable[T]): Cancelable = {
      streamObserver match {
        case _: CallStreamObserver[T] =>
          obs.subscribe(this.toSubscriber)

        case _ => // No back-pressure
          obs
            .doOnError(exception => streamObserver.onError(GRPCErrors.toStatusException(exception)))
            .doOnComplete(() => streamObserver.onCompleted())
            .foreach(value => streamObserver.onNext(value))
      }
    }

    def failWith(error: ApiError): Unit = {
      streamObserver.onError(GRPCErrors.toStatusException(error))
    }
  }

  implicit class VanillaTransactionConversions(tx: VanillaTransaction) {
    def toPB = PBTransactions.protobuf(tx)
  }

  implicit class PBSignedTransactionConversions(tx: PBSignedTransaction) {
    def toVanilla = PBTransactions.vanilla(tx).explicitGet()
  }

  implicit class PBExchangeTransactionConversions(tx: ExchangeTransaction) {
    def toVanilla: Either[ValidationError, exchange.ExchangeTransaction] =
      for {
        fee <- tx.fee.toRight(GenericError("Fee must be specified"))
        r <- {
          val proofs = Proofs(tx.proofs.map(to))
          tx.version match {
            case 1 =>
              exchange.ExchangeTransactionV1.create(
                tx.orders.head.toVanilla.asInstanceOf[exchange.OrderV1], // todo
                tx.orders.last.toVanilla.asInstanceOf[exchange.OrderV1],
                tx.amount,
                tx.price,
                tx.buyMatcherFee,
                tx.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs.toSignature
              )
            case 2 =>
              exchange.ExchangeTransactionV2.create(
                tx.orders.head.toVanilla,
                tx.orders.last.toVanilla,
                tx.amount,
                tx.price,
                tx.buyMatcherFee,
                tx.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs
              )
            case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v") // ??
          }
        }
      } yield r
  }

  implicit class PBOrderConversions(order: Order) {
    def toVanilla: exchange.Order =
      exchange.Order(
        PublicKey(to(order.senderPublicKey)),
        PublicKey(to(order.matcherPublicKey)),
        exchange.AssetPair(Asset.fromProtoId(order.getAssetPair.getAmountAssetId), Asset.fromProtoId(order.getAssetPair.getPriceAssetId)),
        order.orderSide match {
          case Order.Side.BUY             => exchange.OrderType.BUY
          case Order.Side.SELL            => exchange.OrderType.SELL
          case Order.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.getMatcherFee.amount,
        order.proofs.map(_.toByteArray: ByteStr),
        order.version.toByte,
        IssuedAsset(to(order.matcherFeeAssetId))
      )
  }

  implicit class EitherToFutureConversionOps[E, T](either: Either[E, T])(implicit toThrowable: E => Throwable) {
    def toFuture: Future[T] = {
      val result = either.left
        .map(e => GRPCErrors.toStatusException(toThrowable(e)))
        .toTry

      Future.fromTry(result)
    }
  }

  def to(xs: ByteString): ByteStr = ByteStr(xs.toByteArray)
  def to(xs: ByteStr): ByteString = ByteString.copyFrom(xs.arr)
}
