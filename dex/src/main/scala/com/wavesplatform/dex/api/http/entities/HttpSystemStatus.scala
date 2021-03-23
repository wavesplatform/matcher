package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.HttpSystemStatus.Status
import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import com.wavesplatform.dex.meta.getSimpleName
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, Reads, Writes}

case class HttpSystemStatus(
  @ApiModelProperty(
    dataType = "string",
    allowableValues = "Starting, Stopping, Closing, Working"
  ) serviceStatus: Status,
  @ApiModelProperty(
    dataType = "string",
    allowableValues = "Starting, Stopping, Closing, Working"
  ) blockchainStatus: Status
)

object HttpSystemStatus {

  implicit val HttpSystemStatusFormat: Format[HttpSystemStatus] = Json.format

  def from(service: MatcherStatus, blockchain: CombinedStream.Status): HttpSystemStatus = {
    def fromMS(x: MatcherStatus) = x match {
      case MatcherStatus.Starting => Status.Starting
      case MatcherStatus.Stopping => Status.Stopping
      case MatcherStatus.Working => Status.Working
    }

    def fromCS(x: CombinedStream.Status) = x match {
      case CombinedStream.Status.Starting(_, _) => Status.Starting
      case CombinedStream.Status.Stopping(_, _) => Status.Stopping
      case CombinedStream.Status.Working => Status.Working
      case CombinedStream.Status.Closing(_, _) => Status.Closing
    }
    HttpSystemStatus(fromMS(service), fromCS(blockchain))
  }

  sealed abstract class Status extends Product with Serializable {
    val name: String = getSimpleName(this)
  }

  object Status {

    case object Starting extends Status
    case object Stopping extends Status
    case object Closing extends Status
    case object Working extends Status

    val All = List(Starting, Stopping, Closing, Working)

    implicit val format: Format[Status] = Format(
      Reads.StringReads.map { x =>
        All.find(_.name == x) match {
          case Some(r) => r
          case None => throw new IllegalArgumentException(s"Can't parse '$x' as System status")
        }
      },
      Writes.StringWrites.contramap(_.name)
    )

  }

}
