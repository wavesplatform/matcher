package com.wavesplatform.dex.collection

import scala.annotation.tailrec

object ListOps {

  implicit final class Ops[T](val self: List[T]) extends AnyVal {

    /**
     * @return (p(x) == true, rest xs), the order is not guaranteed!
     */
    def splitOnCondUnordered(p: T => Boolean): (List[T], List[T]) = {
      @tailrec def loop(accLeft: List[T]): (List[T], List[T]) = self match {
        case Nil => (accLeft, Nil)
        case x :: xs =>
          val updatedAccLeft = x :: accLeft
          if (p(x)) loop(updatedAccLeft)
          else (updatedAccLeft, xs)
      }

      loop(List.empty)
    }

  }

}
