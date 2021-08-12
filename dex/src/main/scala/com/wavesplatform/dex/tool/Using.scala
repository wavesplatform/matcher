package com.wavesplatform.dex.tool

import scala.util.{Using => ScalaUsing}

object Using {

  implicit final class UsingManagerOps(val manager: ScalaUsing.Manager.type) extends AnyVal {

    def unsafe[A](f: ScalaUsing.Manager => A): A =
      manager.apply(f).get

  }

}
