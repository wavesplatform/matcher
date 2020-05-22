package com.wavesplatform.dex.load.ws


import com.github.andyglow.websocket.util.Uri
import com.github.andyglow.websocket.{Websocket, WebsocketClient}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait WaitForStop[T] {
  val stringUri = Uri(s"ws://devnet2-htz-nbg1-1.wavesnodes.com:6886/ws/v0")

  private val roundtrip = Promise[Unit]()

  def client: WebsocketClient[T]
  lazy val socket: Websocket = client.open()

  def run(): Unit

  def done(): Unit = {
    socket.close andThen {
      case _ => roundtrip.success(())
    }
  }

  def main(args: Array[String]): Unit = {
    run()

    val f = roundtrip.future flatMap (_ => client.shutdownAsync) andThen {
      case Success(_) => println("!!! [closed]")
      case Failure(x) => println("!!! [failure on close]", x)
    }

    Await.result(f, 5000.millis)
  }

}

