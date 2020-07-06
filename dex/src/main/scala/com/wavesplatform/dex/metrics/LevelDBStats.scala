package com.wavesplatform.dex.metrics

import com.wavesplatform.dex.db.leveldb.Key
import kamon.Kamon
import kamon.metric.{MeasurementUnit, Metric}

object LevelDBStats {

  implicit class DbHistogramExt(val h: Metric.Histogram) {
    def recordTagged(key: Key[_], value: Array[Byte]): Unit = recordTagged(key.name, value)
    def recordTagged(tag: String, value: Array[Byte]): Unit = h.withTag("key", tag).record(Option(value).map(_.length.toLong).getOrElse(0))
    def recordTagged(tag: String, totalBytes: Long): Unit   = h.withTag("key", tag).record(totalBytes)
  }

  val read: Metric.Histogram  = Kamon.histogram("dex.db.read", MeasurementUnit.information.bytes)
  val write: Metric.Histogram = Kamon.histogram("dex.db.write", MeasurementUnit.information.bytes)
}
