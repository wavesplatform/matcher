package com.wavesplatform.dex.domain.order

import cats.syntax.either._
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.deser.EntityParser
import com.wavesplatform.dex.domain.bytes.deser.EntityParser.{ConsumedBytesOffset, Stateful}
import com.wavesplatform.dex.domain.utils.PBUtils
import com.wavesplatform.dex.grpc.integration.protobuf.DexToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.protobuf.order.{Order => PbOrder}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval

@ApiModel(value = "Order")
case class OrderV4(
  @ApiModelProperty(
    hidden = true
  ) orderAuthentication: OrderAuthentication,
  @ApiModelProperty(
    value = "Base58 encoded Matcher Public Key",
    dataType = "string",
    example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8",
    required = true
  ) matcherPublicKey: PublicKey,
  assetPair: AssetPair,
  @ApiModelProperty(
    value = "Order type (sell or buy)",
    dataType = "string",
    example = "sell",
    required = true
  ) orderType: OrderType,
  @ApiModelProperty() amount: Long,
  @ApiModelProperty() price: Long,
  @ApiModelProperty() timestamp: Long,
  @ApiModelProperty() expiration: Long,
  @ApiModelProperty() matcherFee: Long,
  @ApiModelProperty(
    name = "matcherFeeAssetId",
    value = "Base58 encoded Matcher fee asset ID. Waves is used if field isn't specified",
    dataType = "string",
    example = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
    required = false
  ) override val feeAsset: Asset
) extends Order {

  @ApiModelProperty(
    value = "Order version, equals to 4",
    dataType = "integer",
    example = "4",
    required = true
  )
  override val version: Byte = 4

  @ApiModelProperty(hidden = true)
  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(PBUtils.encodeDeterministic(copy(orderAuthentication = orderAuthentication.withoutProofs()).toPB))

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBUtils.encodeDeterministic(this.toPB))

}

object OrderV4 extends EntityParser[OrderV4] {

  override private[domain] def statefulParse: Stateful[(OrderV4, ConsumedBytesOffset)] =
    for {
      order <- readRemaining[OrderV4] {
        PBUtils.decode(_, PbOrder)
          .flatMap {
            _.toVanilla
              .leftMap(err => new RuntimeException(err.message))
              .ensure(new RuntimeException("Unexpected order version"))(_.version == 4)
              .map(_.asInstanceOf[OrderV4])
          }
          .fold(throw _, identity)
      }
      offset <- read[ConsumedBytesOffset]
    } yield order -> offset

}
