package com.wavesplatform.dex.domain.crypto

import com.wavesplatform.dex.domain.error.ValidationError.InvalidSignature
import io.swagger.annotations.ApiModelProperty
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait Signed extends Authorized {

  protected val signatureValid: Coeval[Boolean]

  @ApiModelProperty(hidden = true)
  protected val signedDescendants: Coeval[Seq[Signed]] = Coeval(Nil)

  @ApiModelProperty(hidden = true)
  protected val signaturesValidMemoized: Task[Either[InvalidSignature, this.type]] = Signed.validateTask[this.type](this).memoize

  @ApiModelProperty(hidden = true)
  val signaturesValid: Coeval[Either[InvalidSignature, this.type]] =
    Coeval.evalOnce(Await.result(signaturesValidMemoized.runToFuture(Signed.scheduler), Duration.Inf))
}

object Signed {

  type E[A] = Either[InvalidSignature, A]

  private implicit lazy val scheduler: SchedulerService = {
    val parallelism = (Runtime.getRuntime.availableProcessors() / 2).max(1).min(4)
    Scheduler.computation(parallelism, "sig-validator")
  }

  private def validateTask[S <: Signed](signedEntity: S): Task[E[S]] =
    Task {

      import cats.instances.either._
      import cats.instances.list._
      import cats.syntax.traverse._

      if (!signedEntity.signatureValid()) Task.now(Left(InvalidSignature(signedEntity, None)))
      else if (signedEntity.signedDescendants().isEmpty) Task.now(Right(signedEntity))
      else {
        Task
          .wanderUnordered(signedEntity.signedDescendants())(s => s.signaturesValidMemoized)
          .map(_.sequence.map(_ => signedEntity))
      }
    }.flatten
}
