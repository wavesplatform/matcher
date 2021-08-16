package kamon.instrumentation.logback.tools

import ch.qos.logback.classic.pattern.ClassicConverter
import ch.qos.logback.classic.spi.ILoggingEvent
import kamon.Kamon
import kamon.trace.Identifier

class CompactTraceIDConverter extends ClassicConverter {

  override def convert(event: ILoggingEvent): String = {
    val currentSpan = Kamon.currentSpan()
    val traceID = currentSpan.trace.id

    if (traceID == Identifier.Empty) ""
    else s"[${traceID.string}] "
  }

}
