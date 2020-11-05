package com.wavesplatform.dex.collection

import scala.annotation.tailrec

object ListOps {

  implicit final class Ops[T](val self: List[T]) extends AnyVal {

    /**
     * @return (pxs, where p(x) == true, rest xs), pxs is reversed
     */
    def splitOnCondReversed(p: T => Boolean): (List[T], List[T]) = {
      @tailrec def loop(accLeft: List[T]): (List[T], List[T]) = self match {
        case Nil => (accLeft, Nil)
        case x :: xs =>
          if (p(x)) loop(x :: accLeft)
          else (accLeft, xs)
      }

      loop(List.empty)
    }

    def splitOnCond(p: T => Boolean): (List[T], List[T]) = {
      val (t, f) = splitOnCondReversed(p)
      (t.reverse, f)
    }

  }

}
