package com.wavesplatform.dex.grpc.integration.clients

sealed trait RunScriptResult

object RunScriptResult {
  case class ScriptError(message: String) extends RunScriptResult
  case object Denied extends RunScriptResult
  case object Allowed extends RunScriptResult
  case class UnexpectedResult(rawResult: String) extends RunScriptResult
  case class Exception(name: String, message: String) extends RunScriptResult
}
