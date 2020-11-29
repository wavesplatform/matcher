package com.wavesplatform.dex.collection

import scala.annotation.tailrec

// TODO tests
object ListOps {

  implicit final class Ops[T](val self: List[T]) extends AnyVal {

    /**
     * @return (pxs, rest xs), where: pxs is reversed and for each x in pxs p(x) == true
     */
    def splitOnCondReversed(p: T => Boolean): (List[T], List[T]) = {
      @tailrec def loop(rest: List[T], accLeft: List[T]): (List[T], List[T]) = rest match {
        case Nil => (accLeft, Nil)
        case x :: xs =>
          if (p(x)) loop(xs, x :: accLeft)
          else (accLeft, rest)
      }

      loop(self, List.empty)
    }

    def splitOnCond(p: T => Boolean): (List[T], List[T]) = {
      val (t, f) = splitOnCondReversed(p)
      (t.reverse, f)
    }

  }

}
