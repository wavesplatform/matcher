import java.io.File

import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Def, PluginTrigger, Plugins, inTask, taskKey, settingKey}
import sbtdocker.DockerPlugin
import sbtdocker.DockerPlugin.autoImport._

// TODO move to waves-integration-it
object WavesExtensionDockerPlugin extends AutoPlugin {

  import WavesExtensionDockerKeys._

  override def requires: Plugins      = JvmPlugin && DockerPlugin
  override def trigger: PluginTrigger = PluginTrigger.NoTrigger

  override def projectSettings: Seq[Def.Setting[_]] = inTask(docker)(
    Seq(
      additionalFiles := Seq.empty,
      exposedPorts := Set.empty,
      baseImage := s"wavesplatform/wavesnode:${wavesNodeVersion.value}",
      dockerfile := new Dockerfile {
        from(baseImage.value)
        // see https://github.com/wavesplatform/Waves/blob/master/docker/Dockerfile
        user("root:root")
        add(additionalFiles.value, "/opt/waves/")
        expose(exposedPorts.value.toSeq: _*)
        entryPoint("/opt/waves/start-waves.sh")
      },
      buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
    )
  )
}

object WavesExtensionDockerKeys {
  val additionalFiles  = taskKey[Seq[File]]("Additional files to copy to /opt/waves")
  val exposedPorts     = taskKey[Set[Int]]("Exposed ports")
  val baseImage        = taskKey[String]("A base image for this container")
  val wavesNodeVersion = settingKey[String]("A version of Waves Node")
}
