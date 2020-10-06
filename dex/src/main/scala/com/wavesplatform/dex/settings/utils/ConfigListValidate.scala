package com.wavesplatform.dex.settings.utils

import pureconfig.ConfigListCursor
import pureconfig.error.ConfigReaderFailure
import pureconfig.generic.ProductHint

class ConfigListValidate[ListT] {

  def mk(f: ListT => Option[String]): (ListT, ConfigListCursor, ProductHint[ListT]) => Option[ConfigReaderFailure] =
    (obj: ListT, c: ConfigListCursor, _: ProductHint[ListT]) => f(obj).map(RawFailureReason).map(c.failureFor)

}
