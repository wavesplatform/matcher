package akka.actor.typed.scaladsl

import akka.actor.typed.Behavior
import akka.actor.typed.internal.{BehaviorImpl, StashBufferImplWithCtxPropagation}

object BehaviorsImplicits {

  implicit class BehaviorsOps(val behaviors: Behaviors.type) extends AnyVal {

    def stashWithCtxPropagation[T](capacity: Int)(factory: StashBuffer[T] => Behavior[T]): Behavior[T] =
      BehaviorImpl.DeferredBehavior { ctx =>
        val stash = StashBufferImplWithCtxPropagation[T](ctx, capacity)
        factory(stash)
      }

  }

}
