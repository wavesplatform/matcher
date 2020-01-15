package com.wavesplatform.dex.it.test

import java.nio.charset.StandardCharsets

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base64

import scala.reflect.ClassTag

object Scripts {
  val alwaysTrue: ByteStr  = fromBase64("AgZ7TN8j")
  val alwaysFalse: ByteStr = fromBase64("AgeJ1sz7")

  def fromBase64(x: String): ByteStr = ByteStr.decodeBase64(x).get

  def renderScriptTemplate(templateInBase64: String, replaces: (String, ByteStr)*): ByteStr = {
    val binaryCode = Base64.decode(templateInBase64)
    val r          = replaces.foldLeft(binaryCode) { case (r, (rawVariable, by)) => renderScriptTemplate(r, rawVariable, by.arr) }
    ByteStr(r)
  }

  /**
    * @param rawVariable templateInBase64 should contain encoded text: {rawVariable}
    */
  private def renderScriptTemplate(binaryCode: Array[Byte], rawVariable: String, by: Array[Byte]): Array[Byte] =
    replaceFirst(binaryCode, rawVariable.getBytes(StandardCharsets.UTF_8), by)
      .getOrElse(throw new RuntimeException(s"Can't replace '$rawVariable'"))

  private def replaceFirst[T: ClassTag](where: Array[T], what: Array[T], by: Array[T]): Option[Array[T]] = {
    val i = where.indexOfSlice(what)
    if (i == -1) None
    else
      Some(
        Array.concat(
          where.slice(0, i),
          by,
          where.drop(i + what.length)
        ))
  }
}
