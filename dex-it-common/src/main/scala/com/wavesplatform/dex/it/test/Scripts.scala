package com.wavesplatform.dex.it.test

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base64
import com.wavesplatform.dex.domain.crypto.secureHash

object Scripts {
  val alwaysTrue: ByteStr = fromBase64("AgZ7TN8j")
  val alwaysFalse: ByteStr = fromBase64("AgeJ1sz7")

  def fromBase64(x: String): ByteStr = ByteStr.decodeBase64(x).get

  /**
   *  @note Works only with "let" and base64/base58 !
   */
  def renderScriptTemplate(templateInBase64: String, replaces: (String, ByteStr)*): ByteStr = {
    val binaryCode = Base64.decode(templateInBase64).dropRight(4)
    val withoutChecksum = replaces.foldLeft(binaryCode) { case (r, (rawVariable, by)) => renderScriptTemplate(r, rawVariable, by.arr) }
    ByteStr(
      Array.concat(
        withoutChecksum,
        secureHash(withoutChecksum).take(4)
      )
    )
  }

  /**
   * @param rawVariable templateInBase64 should contain encoded text: {rawVariable}
   * @note Works only with "let" and base64/base58 !
   */
  private def renderScriptTemplate(binaryCode: Array[Byte], rawVariable: String, by: Array[Byte]): Array[Byte] =
    replaceFirst(binaryCode, rawVariable.getBytes(StandardCharsets.UTF_8), by)
      .getOrElse(throw new RuntimeException(s"Can't replace '$rawVariable'"))

  private def replaceFirst(where: Array[Byte], what: Array[Byte], by: Array[Byte]): Option[Array[Byte]] = {
    val i = where.indexOfSlice(what)
    if (i == -1) None
    else
      Some(
        Array.concat(
          where.slice(0, i - 4),
          Ints.toByteArray(by.length),
          by,
          where.drop(i + what.length)
        )
      )
  }

}
