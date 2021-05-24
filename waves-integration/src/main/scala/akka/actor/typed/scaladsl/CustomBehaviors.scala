package akka.actor.typed.scaladsl

import akka.actor.typed.Behavior
import akka.actor.typed.internal.{BehaviorImpl, StashBufferImplWithCtxPropagation}

object CustomBehaviors {

  def stashWithCtxPropagation[T](capacity: Int)(factory: StashBuffer[T] => Behavior[T]): Behavior[T] =
    BehaviorImpl.DeferredBehavior { ctx =>
      val stash = StashBufferImplWithCtxPropagation[T](ctx, capacity)
      factory(stash)
    }

}
