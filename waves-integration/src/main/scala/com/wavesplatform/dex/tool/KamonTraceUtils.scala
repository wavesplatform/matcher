package com.wavesplatform.dex.tool

import kamon.Kamon
import kamon.context.{BinaryPropagation, Context}
import kamon.trace.Span
import kamon.trace.Trace.SamplingDecision
import kamon.trace.Tracer.PreFinishHook

import java.io.ByteArrayOutputStream

object KamonTraceUtils {

  def setSpanName(name: String): Unit =
    Kamon.currentSpan().name(name)

  def setSpanNameAndForceSamplingDecision(name: String): Unit = {

    //initially span has Unknown state and there is no 100% guarantee that it will be logged in jaeger
    //so we have to force sampling decision (apply our sampler to a span) to force its state
    //Unknown -> Sample
    //Unknown -> DoNotSample
    //in case when a span is marked as Sample it will be sent in jaeger.
    Kamon.currentSpan().takeSamplingDecision()

    Kamon.currentSpan().name(name)
  }

  def runWithIgnoredSpan[A](f: => A): A = {
    val span = mkIgnoredSpan()
    Kamon.runWithSpan(span)(f)
  }

  def writeCtx(ctx: Context): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    Kamon.defaultBinaryPropagation().write(ctx, BinaryPropagation.ByteStreamWriter.of(out))
    out.toByteArray
  }

  def readCtx(input: Array[Byte]): Context =
    Kamon.defaultBinaryPropagation().read(BinaryPropagation.ByteStreamReader.of(input))

  lazy val DoNotSample: SamplingDecision = loadModule(mkSamplingDecisionPath("DoNotSample"))

  lazy val Sample: SamplingDecision = loadModule(mkSamplingDecisionPath("Sample"))

  lazy val FollowsFrom: Span.Link.Kind = loadModule("kamon.trace.Span$Link$Kind$FollowsFrom$")

  private def mkSamplingDecisionPath(name: String): String =
    "kamon.trace.Trace$SamplingDecision$" + name + "$"

  //for some reason accessing sampling decisions and links fails scalac
  //this is a hacky workaround
  private def loadModule[A](path: String): A =
    //https://stackoverflow.com/questions/8867766/scala-dynamic-object-class-loading
    Class.forName(path).getField("MODULE$").get(null).asInstanceOf[A]

  private def mkIgnoredSpan(): Span =
    Kamon.spanBuilder("ignored")
      .ignoreParentFromContext()
      .doNotTrackMetrics()
      .samplingDecision(DoNotSample)
      .traceId(Kamon.currentSpan().trace.id)
      .start()

  final class FilteringRejectedHook extends PreFinishHook {

    override def beforeFinish(span: Span): Unit =
      if (span.operationName() == "/rejected")
        span.trace.drop()

  }

}
