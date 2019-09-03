package com.wavesplatform

//import cats.Monad
//import cats.instances.try_._
//import cats.syntax.monad._
//import com.wavesplatform.common.meta.traced
import com.wavesplatform.it.api.Logger

import scala.util.{Success, Try}

trait Bar {}

//@traced
trait Foo[F[_]] extends Bar {
  val x = 1
  def run(x: Int): F[String]
  def get(): F[Int]
}

trait WithTraced {
  def traced[F](delegate: F): F = ???
}

object Foo extends WithTraced {
  def test: Int = 10
}

object TestTest extends App {
  implicit val logger = new Logger[Try] {
    override def error(msg: => String): Try[Unit] = ???
    override def error(msg: => String, cause: Throwable): Try[Unit] = ???
    override def warn(msg: => String): Try[Unit] = ???
    override def info(msg: => String): Try[Unit] = ???
    override def debug(msg: => String): Try[Unit] = Success(println(msg))
    override def trace(msg: => String): Try[Unit] = ???
  }

  val x = new Foo[Try] {
    override def run(x: Int): Try[String] = Success("run")
    override def get(): Try[Int]          = Success(42)
  }

  //_root_.cats.Monad[Try].flatMap()
  val wrappedX = Foo.traced(x)
  println(wrappedX.get())
}
