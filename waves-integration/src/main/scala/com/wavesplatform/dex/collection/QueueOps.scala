package com.wavesplatform.dex.collection

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// TODO DEX-994, tests
object QueueOps {

  implicit final class Ops[T](val self: Queue[T]) extends AnyVal {

    /**
     * @return (pxs, rest xs), where: pxs is reversed and for each x in pxs p(x) == true
     */
    def splitOnCondReversed(p: T => Boolean): (List[T], Queue[T]) = {
      @tailrec def loop(rest: Queue[T], accDropped: List[T]): (List[T], Queue[T]) = rest.headOption match {
        case None => (accDropped, rest)
        case Some(x) =>
          if (p(x)) loop(rest.tail, x :: accDropped)
          else (accDropped, rest)
      }

      loop(self, List.empty)
    }

    def splitOnCond(p: T => Boolean): (List[T], Queue[T]) = {
      val (t, f) = splitOnCondReversed(p)
      (t.reverse, f)
    }

  }

}
