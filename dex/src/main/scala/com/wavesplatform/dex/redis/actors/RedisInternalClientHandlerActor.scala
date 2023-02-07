package com.wavesplatform.dex.redis.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.wavesplatform.dex.api.ws.actors.WsInternalClientHandlerActor
import com.wavesplatform.dex.api.ws.actors.WsInternalClientHandlerActor.Message
import com.wavesplatform.dex.api.ws.protocol.WsServerMessage.wsServerMessageWrites
import com.wavesplatform.dex.redis.RedisClient
import com.wavesplatform.dex.settings.RedisInternalClientHandlerActorSettings
import org.redisson.client.codec.StringCodec
import play.api.libs.json.Json

import scala.util.{Failure, Success}

object RedisInternalClientHandlerActor {

  def apply(
    settings: RedisInternalClientHandlerActorSettings,
    redisClient: RedisClient
  ): Behavior[WsInternalClientHandlerActor.Message] =
    Behaviors.setup[Message] { ctx =>
      import ctx.executionContext

      val log = ctx.log
      val stream = redisClient.getStream[String, String](settings.streamName, StringCodec.INSTANCE)

      Behaviors.receiveMessage {
        case WsInternalClientHandlerActor.Command.ForwardToClient(wsMessage) =>
          val jsonEncoded = Json.stringify(Json.toJson(wsMessage))
          stream.addOrderedAsync(settings.key, jsonEncoded).onComplete {
            case Failure(exception) =>
              log.warn("Couldn't save message {}, error {}", jsonEncoded, exception)
            case Success(_) =>
              ()
          }

          Behaviors.same

        case x =>
          log.warn("Can't handle command {}", x)
          Behaviors.same
      }

    }

}
