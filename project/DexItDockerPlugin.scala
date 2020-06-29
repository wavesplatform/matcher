import java.io.File

import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Def, LocalProject, PluginTrigger, Plugins, inTask, taskKey}
import sbtdocker.DockerPlugin
import sbtdocker.DockerPlugin.autoImport._

object DexItDockerPlugin extends AutoPlugin {

  import DexItDockerKeys._

  override def requires: Plugins      = JvmPlugin && DockerPlugin
  override def trigger: PluginTrigger = PluginTrigger.NoTrigger

  override def projectSettings: Seq[Def.Setting[_]] =
    inTask(docker)(
      Seq(
        additionalFiles := Seq.empty,
        exposedPorts := Set(6886),
        baseImage := "com.wavesplatform/waves-dex:latest",
        dockerfile := {
          new Dockerfile {
            from(baseImage.value)
            add(additionalFiles.value, "/opt/waves-dex/")
            expose(exposedPorts.value.toSeq: _*)
          }
        },
        buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
      )
    ) ++ Seq(docker := docker.dependsOn(LocalProject("dex") / docker).value)
}

object DexItDockerKeys {
  val additionalFiles = taskKey[Seq[File]]("Additional files to copy to /opt/waves-dex")
  val exposedPorts    = taskKey[Set[Int]]("Exposed ports")
  val baseImage       = taskKey[String]("A base image for this container")
}
