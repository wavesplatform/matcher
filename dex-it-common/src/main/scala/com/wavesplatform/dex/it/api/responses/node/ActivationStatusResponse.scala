package com.wavesplatform.dex.it.api.responses.node

import play.api.libs.json._

case class ActivationStatusResponse(features: Seq[ActivationStatusResponse.FeatureStatus])

object ActivationStatusResponse {
  case class FeatureStatus(id: Short, blockchainStatus: FeatureStatus.BlockchainStatus)

  object FeatureStatus {
    sealed trait BlockchainStatus

    object BlockchainStatus {
      case object Undefined extends BlockchainStatus
      case object Approved extends BlockchainStatus
      case object Activated extends BlockchainStatus

      implicit val blockchainFeatureStatusReads: Reads[BlockchainStatus] = Reads {
        case JsString(x) =>
          x match {
            case "VOTING" => JsSuccess(Undefined)
            case "APPROVED" => JsSuccess(Approved)
            case "ACTIVATED" => JsSuccess(Activated)
            case _ => JsError(s"Unknown feature status: $x")
          }
        case x => JsError(s"Can't parse '$x' as BlockchainStatus'")
      }

    }

    implicit val featureStatusReads: Reads[FeatureStatus] = Json.reads[FeatureStatus]
  }

  implicit val activationStatusResponseReads: Reads[ActivationStatusResponse] = Json.reads[ActivationStatusResponse]
}
