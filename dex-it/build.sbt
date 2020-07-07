enablePlugins(ItTestPlugin, JvmPlugin, sbtdocker.DockerPlugin)

description := "DEX integration tests"

libraryDependencies ++= Dependencies.Module.dexIt

// this image hasn't a config file
docker := docker.dependsOn(LocalProject("waves-integration-it") / docker).value
inTask(docker)(
  Seq(
    imageNames := Seq(ImageName("com.wavesplatform/dex-it:latest")),
    dockerfile := new Dockerfile {

      val yourKitArchive = "YourKit-JavaProfiler-2019.8-docker.zip"
      val basePath       = "/opt/waves-dex"
      val entryPointSh   = s"$basePath/start.sh"

      from("com.wavesplatform/matcherserver:latest")

      // See https://www.yourkit.com/docs/java/help/docker.jsp
      runRaw(s"""mkdir -p /opt/waves-dex && \\
                |apk update && \\
                |apk add --no-cache openssl ca-certificates && \\
                |wget --quiet "https://search.maven.org/remotecontent?filepath=org/aspectj/aspectjweaver/1.9.1/aspectjweaver-1.9.1.jar" -O /opt/waves-dex/aspectjweaver.jar && \\
                |wget --quiet "https://www.yourkit.com/download/docker/$yourKitArchive" -P /tmp/ && \\
                |unzip /tmp/$yourKitArchive -d /usr/local && \\
                |rm -f /tmp/$yourKitArchive""".stripMargin)

      add(
        sources = Seq(
          (Test / sourceDirectory).value / "container" / "start.sh",
          (Test / resourceDirectory).value / "dex-servers" / "logback-container.xml"
        ),
        destination = s"$basePath/"
      )
      runShell("chmod", "+x", entryPointSh)
      entryPoint(entryPointSh)
      expose(10001) // Profiler
    },
    buildOptions := BuildOptions(removeIntermediateContainers = BuildOptions.Remove.OnSuccess)
  )
) ++ Seq(docker := docker.dependsOn(LocalProject("dex") / docker).value)
