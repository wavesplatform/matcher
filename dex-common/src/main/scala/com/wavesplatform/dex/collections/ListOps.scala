package com.wavesplatform.dex.collections

import scala.annotation.tailrec

// TODO DEX-994, tests
object ListOps {

  implicit final class Ops[T](val self: List[T]) extends AnyVal {

    /**
     * @return (pxs, rest xs), where: pxs is reversed and for each x in pxs p(x) == true
     */
    def splitOnCondReversed(p: T => Boolean): (List[T], List[T]) = {
      @tailrec def loop(rest: List[T], accDropped: List[T]): (List[T], List[T]) = rest match {
        case Nil => (accDropped, Nil)
        case x :: xs =>
          if (p(x)) loop(xs, x :: accDropped)
          else (accDropped, rest)
      }

      loop(self, List.empty)
    }

    def splitOnCond(p: T => Boolean): (List[T], List[T]) = {
      val (t, f) = splitOnCondReversed(p)
      (t.reverse, f)
    }

  }

  implicit final class ListOfMapsOps[K, V](val self: List[Map[K, V]]) extends AnyVal {

    /**
     * List(Map(a -> 1, b -> 2), Map(a -> 4, d -> 3)).foldSkipped == Map(a -> 1, b -> 2, d -> 3)
     */
    def foldSkipped: Map[K, V] = self.foldLeft(Map.empty[K, V]) { case (r, xs) =>
      xs.foldLeft(r) { case (r, (k, v)) =>
        if (r.contains(k)) r
        else r.updated(k, v)
      }
    }

  }

}
