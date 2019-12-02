import java.io.File

import DexDockerKeys._
import sbt.plugins.JvmPlugin
import sbt.{AutoPlugin, Def, Plugins, inTask, taskKey}
import sbtdocker.DockerPlugin
import sbtdocker.DockerPlugin.autoImport._

object DexDockerPlugin extends AutoPlugin {
  override def requires: Plugins = JvmPlugin && DockerPlugin

  override def projectSettings: Seq[Def.Setting[_]] =
    inTask(docker)(
      Seq(
        imageNames := Seq(ImageName("com.wavesplatform/waves-dex:latest")),
        additionalFiles := Seq.empty,
        exposedPorts := Set(6886),
        baseImage := "anapsix/alpine-java:8_server-jre",
        dockerfile := new Dockerfile {
          from(baseImage.value)
          val bin = "/opt/waves-dex/start.sh"

          runRaw(s"""mkdir -p /opt/waves-dex && \\
                    |apk update && \\
                    |apk add --no-cache openssl ca-certificates""".stripMargin)

          add(additionalFiles.value, "/opt/waves-dex/")
          runShell("chmod", "+x", bin)
          entryPoint(bin)
          expose(exposedPorts.value.toSeq: _*)
        },
        buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
      ))
}

object DexDockerKeys {
  val additionalFiles = taskKey[Seq[File]]("Additional files to copy to /opt/waves-dex/")
  val exposedPorts    = taskKey[Set[Int]]("Exposed ports")
  val baseImage       = taskKey[String]("A base image for this container")
}
