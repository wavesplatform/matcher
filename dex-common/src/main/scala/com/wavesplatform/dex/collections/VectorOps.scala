package com.wavesplatform.dex.collections

import scala.annotation.tailrec

// TODO DEX-994, tests
object VectorOps {

  implicit final class Ops[T](val self: Vector[T]) extends AnyVal {

    /**
     * @return (pxs, rest xs), where: pxs is reversed and for each x in pxs p(x) == true
     */
    def splitOnCondReversed(p: T => Boolean): (List[T], Vector[T]) = {
      @tailrec def loop(rest: Vector[T], accDropped: List[T]): (List[T], Vector[T]) = rest.headOption match {
        case None => (accDropped, rest)
        case Some(x) =>
          if (p(x)) loop(rest.tail, x :: accDropped)
          else (accDropped, rest)
      }

      loop(self, List.empty)
    }

    def splitOnCond(p: T => Boolean): (List[T], Vector[T]) = {
      val (t, f) = splitOnCondReversed(p)
      (t.reverse, f)
    }

  }

}
