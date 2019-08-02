import java.io.File

import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Def, PluginTrigger, Plugins, inTask, taskKey}
import sbtdocker.DockerPlugin
import sbtdocker.DockerPlugin.autoImport._

object WavesExtensionDockerPlugin extends AutoPlugin {

  import WavesExtensionDockerKeys._

  override def requires: Plugins      = JvmPlugin && DockerPlugin
  override def trigger: PluginTrigger = PluginTrigger.NoTrigger

  override def projectSettings: Seq[Def.Setting[_]] =
    inTask(docker)(
      Seq(
        additionalFiles := Seq.empty,
        exposedPorts := Set.empty,
        baseImage := "com.wavesplatform/node-it:latest",
        dockerfile := {
          new Dockerfile {
            from(baseImage.value)
            add(additionalFiles.value, "/opt/waves/")
            expose(exposedPorts.value.toSeq: _*)
          }
        },
        buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess),
        docker := docker.dependsOn(buildNodeContainer).value
      ))
}

object WavesExtensionDockerKeys {
  val additionalFiles    = taskKey[Seq[File]]("Additional files to copy to /opt/waves")
  val exposedPorts       = taskKey[Set[Int]]("Exposed ports")
  val buildNodeContainer = taskKey[Unit]("Builds a NODE container")
  val baseImage          = taskKey[String]("A base image for this container")
}
