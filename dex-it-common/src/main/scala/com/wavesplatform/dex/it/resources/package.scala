package com.wavesplatform.dex.it

import java.io.FileNotFoundException

import scala.io.Source
import scala.util.Try

package object resources {
  def getRawContentFromResource(fileName: String): String =
    Try(Source fromResource fileName).getOrElse { throw new FileNotFoundException(s"Resource '$fileName'") }.mkString
}
