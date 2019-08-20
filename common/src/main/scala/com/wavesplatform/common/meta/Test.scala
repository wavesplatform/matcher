package com.wavesplatform.common.meta

class Test {
  def foo(x: Int): Unit = println(s"foo(x=$x)")
}

object Test {
  def w(x: Test) = Traced.wrap[Test]()
}
