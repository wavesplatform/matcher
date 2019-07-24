package com.wavesplatform.dex.grpc.integration.services

import java.util.concurrent.ConcurrentHashMap

import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.dto.{BalanceChangesResponse => VBalanceChangesResponse}
import com.wavesplatform.dex.grpc.integration.protobuf.BalancesServiceConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.Implicits._
import com.wavesplatform.dex.grpc.integration.services.CurrentObservableBalancesResponse.AssetsSet
import com.wavesplatform.extensions.{Context => ExtensionContext}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import mouse.any._

import scala.collection.JavaConverters._
import scala.concurrent.Future

class BalancesServiceGrpcImpl(context: ExtensionContext)(implicit sc: Scheduler) extends BalancesServiceGrpc.BalancesService with ScorexLogging {

  val observableBalances   = new ConcurrentHashMap[Address, Set[Asset]](1000, 0.9f, 10)
  val responseObserversSet = scala.collection.mutable.Set.empty[StreamObserver[BalanceChangesResponse]]

  private def isObservable(address: Address, asset: Asset): Boolean = Option { observableBalances.get(address) } exists { _.contains(asset) }

  context.spendableBalanceChanged
    .doOnComplete { () =>
      responseObserversSet.foreach(_.onCompleted)
    }
    .foreach {
      case (address, asset) =>
        if (isObservable(address, asset)) responseObserversSet foreach {
          _.onNext { protobuf(VBalanceChangesResponse(address, asset, context.blockchain.balance(address, asset))) }
        }
    }

  override def getBalanceChanges(responseObserver: StreamObserver[BalanceChangesResponse]): StreamObserver[BalanceChangesRequest] = {

    responseObserversSet += responseObserver

    new StreamObserver[BalanceChangesRequest] {
      override def onError(t: Throwable): Unit = Unit
      override def onCompleted(): Unit         = Unit
      override def onNext(value: BalanceChangesRequest): Unit = {
        observableBalances.merge(value.address.toVanillaAddress, Set(value.getAsset.toVanillaAsset), _ ++ _)
      }
    }
  }

  override def getCurrentObservableBalances(request: Empty): Future[CurrentObservableBalancesResponse] = {
    observableBalances.asScala.map { case (address, assets) => address.toString -> AssetsSet(assets.map(_.toPBAsset.get).toSeq) }.toMap |> CurrentObservableBalancesResponse.apply |> Future.successful
  }
}
