package com.wavesplatform.it.sync

import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.it.MatcherSuiteBase

final class ReBroadcastingFailedByScriptTxsTestSuite extends MatcherSuiteBase {

  "ReBroadcastingFailedByScriptTxsTestSuite" - {

    "should not rebroadcast transactions failed by account script" in {

    }
  }

  /*
  {-# STDLIB_VERSION 5 #-}
  {-# CONTENT_TYPE EXPRESSION #-}
  {-# SCRIPT_TYPE ACCOUNT #-}

  match (tx) {
    case _: SetScriptTransaction => true
    case _: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
    case _ => false
  }
   */
  private lazy val defaultScript =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAABAAAAA" +
      "gUAAAAHJG1hdGNoMAIAAAAFT3JkZXIJAAH0AAAAAwgFAAAAAnR4AAAACWJvZHlCeXRlcwkAAZEAAAACCAUAAAACdHgAAAAGcHJv" +
      "b2ZzAAAAAAAAAAAACAUAAAACdHgAAAAPc2VuZGVyUHVibGljS2V5BwPe+og="
    )

  /*
  {-# STDLIB_VERSION 5 #-}
  {-# CONTENT_TYPE EXPRESSION #-}
  {-# SCRIPT_TYPE ACCOUNT #-}
  match tx {
    case _: SetScriptTransaction => true
    case _: Order  => sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) && height > 1000
    case _ => false
  }
   */
  private lazy val scriptHeightGt1000 =
    Scripts.fromBase64(
      "BQQAAAAHJG1hdGNoMAUAAAACdHgDCQAAAQAAAAIFAAAAByRtYXRjaDACAAAAFFNldFNjcmlwdFRyYW5zYWN0aW9uBgMJAAABAAAAAgUAA" +
      "AAHJG1hdGNoMAIAAAAFT3JkZXIDCQAB9AAAAAMIBQAAAAJ0eAAAAAlib2R5Qnl0ZXMJAAGRAAAAAggFAAAAAnR4AAAABnByb29mcwAAA" +
      "AAAAAAAAAgFAAAAAnR4AAAAD3NlbmRlclB1YmxpY0tleQkAAGYAAAACBQAAAAZoZWlnaHQAAAAAAAAAA+gHB4Yt2cc="
    )

}
