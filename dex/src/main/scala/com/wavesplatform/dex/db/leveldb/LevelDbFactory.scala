package com.wavesplatform.dex.db.leveldb

import com.wavesplatform.dex.domain.utils.ScorexLogging
import org.iq80.leveldb.DBFactory

import scala.util.Try

object LevelDbFactory extends ScorexLogging {
  private[this] val jnaFactory = "com.wavesplatform.dex.db.leveldb.jna.LevelDBJNADBFactory"
  private[this] val jniFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private[this] val javaFactory = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val isTesting = sys.props.get("sbt-testing").isDefined
    val nativeFactories = if (isTesting) List.empty else List(jnaFactory, jniFactory)
    val allFactories = nativeFactories :+ javaFactory

    val pairs = for {
      loader <- List(ClassLoader.getSystemClassLoader, getClass.getClassLoader).view
      factoryName <- allFactories
      factory <- Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory]).toOption
    } yield (factoryName, factory)

    val (fName, factory) = pairs.headOption.getOrElse(throw new Exception(s"Could not load any of the factory classes: $allFactories"))
    if (fName == jniFactory) log.warn("Using the old LevelDB JNI implementation")
    if (fName == javaFactory) log.warn("Using the pure java LevelDB implementation which is still experimental")
    else log.info(s"Loaded $fName with $factory")
    factory
  }

}
