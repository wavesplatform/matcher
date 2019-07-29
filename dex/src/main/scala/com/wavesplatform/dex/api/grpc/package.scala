package com.wavesplatform.dex.api

import java.util.concurrent.atomic.AtomicReference

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.api.grpc.GRPCErrors
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{Amount, AssetId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.{Asset, Proofs}
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

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

  implicit class VanillaTransactionConversions(tx: exchange.ExchangeTransaction) {
    def toPB: ExchangeTransaction = ExchangeTransaction(
      chainId = tx.chainByte.getOrElse(AddressScheme.current.chainId).toInt,
      senderPublicKey = ByteString.copyFrom(tx.sender.bytes.arr),
      fee = Some(Amount(assetId = None, amount = tx.fee)),
      timestamp = tx.timestamp,
      version = tx.version,
      data = ExchangeTransaction.Data.Exchange(
        ExchangeTransactionData(
          amount = tx.amount,
          price = tx.price,
          buyMatcherFee = tx.buyMatcherFee,
          sellMatcherFee = tx.sellMatcherFee,
          orders = Seq(tx.buyOrder.toPB, tx.sellOrder.toPB)
        ))
    )
  }

  implicit class PBExchangeTransactionConversions(self: SignedExchangeTransaction) {
    def toVanilla: Either[ValidationError, exchange.ExchangeTransaction] =
      for {
        tx <- self.transaction.fold[Either[ValidationError, ExchangeTransaction]](GenericError("The transaction must be specified").asLeft)(_.asRight)
        data <- tx.data.exchange
          .fold[Either[ValidationError, ExchangeTransactionData]](GenericError("The transaction's data must be specified").asLeft)(_.asRight)
        fee <- tx.fee.toRight(GenericError("The fee must be specified"))
        r <- {
          val proofs = Proofs(self.proofs.map(to))
          tx.version match {
            case 1 =>
              exchange.ExchangeTransactionV1.create(
                data.orders.head.toVanilla.asInstanceOf[exchange.OrderV1], // todo
                data.orders.last.toVanilla.asInstanceOf[exchange.OrderV1],
                data.amount,
                data.price,
                data.buyMatcherFee,
                data.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs.toSignature
              )
            case 2 =>
              exchange.ExchangeTransactionV2.create(
                data.orders.head.toVanilla,
                data.orders.last.toVanilla,
                data.amount,
                data.price,
                data.buyMatcherFee,
                data.sellMatcherFee,
                fee.amount,
                tx.timestamp,
                proofs
              )
            case v => throw new IllegalArgumentException(s"Unsupported transaction version: $v") // ??
          }
        }
      } yield r
  }

  implicit class VanillaAssetId(self: Asset) {
    def toPB: AssetId = self match {
      case Asset.IssuedAsset(assetId) => AssetId().withIssuedAsset(to(assetId))
      case Asset.Waves                => AssetId().withWaves(com.google.protobuf.empty.Empty())
    }
  }

  implicit class VanillaOrderConversions(order: exchange.Order) {
    def toPB: Order = Order(
      chainId = 0,
      ByteString.copyFrom(order.senderPublicKey),
      ByteString.copyFrom(order.matcherPublicKey),
      Some(Order.AssetPair(Some(order.assetPair.amountAsset.protoId), Some(order.assetPair.priceAsset.protoId))),
      order.orderType match {
        case exchange.OrderType.BUY  => Order.Side.BUY
        case exchange.OrderType.SELL => Order.Side.SELL
      },
      order.amount,
      order.price,
      order.timestamp,
      order.expiration,
      Some(Amount(Some(order.matcherFeeAssetId.toPB), order.matcherFee)),
      order.version,
      order.proofs.map(to)
    )
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
