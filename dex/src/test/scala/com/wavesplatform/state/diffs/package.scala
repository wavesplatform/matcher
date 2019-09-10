package com.wavesplatform.state

import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.db.WithState
import org.scalatest.Matchers

package object diffs extends WithState with Matchers {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
