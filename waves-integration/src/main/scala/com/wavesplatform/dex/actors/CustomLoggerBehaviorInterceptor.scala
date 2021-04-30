package com.wavesplatform.dex.actors

import akka.actor.typed.BehaviorInterceptor.ReceiveTarget
import akka.actor.typed.{Behavior, BehaviorInterceptor, TypedActorContext}
import com.wavesplatform.dex.actors.CustomLoggerBehaviorInterceptor.{LogMessageTemplate, Settings}
import org.slf4j.Logger
import org.slf4j.event.Level

import scala.reflect.ClassTag

class CustomLoggerBehaviorInterceptor[T](private val opts: Settings[T])(implicit ct: ClassTag[T]) extends BehaviorInterceptor[T, T] {

  import opts.logger
  private val formatter = opts.formatter.lift

  override def aroundReceive(ctx: TypedActorContext[T], msg: T, target: ReceiveTarget[T]): Behavior[T] = {
    log(LogMessageTemplate, msg)
    target(ctx, msg)
  }

  private def log(template: String, message: T): Unit =
    if (opts.enabled) formatter(message).foreach { message =>
      opts.level match {
        case Level.ERROR => logger.error(template, message)
        case Level.WARN => logger.warn(template, message)
        case Level.INFO => logger.info(template, message)
        case Level.DEBUG => logger.debug(template, message)
        case Level.TRACE => logger.trace(template, message)
        case other => throw new IllegalArgumentException(s"Unknown log level [$other].")
      }
    }

  override def isSame(other: BehaviorInterceptor[Any, Any]): Boolean = other match {
    case a: CustomLoggerBehaviorInterceptor[_] => a.opts == opts
    case _ => false
  }

}

object CustomLoggerBehaviorInterceptor {
  val LogMessageTemplate = "Got: {}"

  case class Settings[T](logger: Logger, enabled: Boolean, level: Level, formatter: PartialFunction[T, String])
}
