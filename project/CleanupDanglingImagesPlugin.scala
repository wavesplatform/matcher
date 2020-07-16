import sbt.Keys.streams
import sbt.{AutoPlugin, Def, PluginTrigger, Plugins, taskKey}

import scala.sys.process._

object CleanupDanglingImagesPlugin extends AutoPlugin {

  object autoImport extends CleanupDanglingImagesKeys
  import autoImport._

  override val trigger = PluginTrigger.NoTrigger

  override def requires: Plugins = sbtdocker.DockerPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    cleanupDandlingImages := {
      streams.value.log.info(s"Cleaning dangling images...")
      "docker image prune -f".!
    }
  )
}

trait CleanupDanglingImagesKeys {
  val cleanupDandlingImages = taskKey[Unit]("Cleanup dangling images")
}
