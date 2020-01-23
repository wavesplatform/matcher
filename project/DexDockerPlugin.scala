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
        exposedPorts := Set(
          6886, // REST API
          10001 // Profiler
        ),
        baseImage := "anapsix/alpine-java:8_server-jre",
        dockerfile := new Dockerfile {
          from(baseImage.value)
          val yourKitArchive = "YourKit-JavaProfiler-2019.8-docker.zip"
          val bin            = "/opt/waves-dex/start.sh"

          // See https://www.yourkit.com/docs/java/help/docker.jsp
          runRaw(s"""mkdir -p /opt/waves-dex && \\
                    |apk update && \\
                    |apk add --no-cache openssl ca-certificates && \\
                    |wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O /opt/waves-dex/aspectjweaver.jar && \\
                    |wget --quiet "https://www.yourkit.com/download/docker/$yourKitArchive" -P /tmp/ && \\
                    |unzip /tmp/$yourKitArchive -d /usr/local && \\
                    |rm -f /tmp/$yourKitArchive""".stripMargin)

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
