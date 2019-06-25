import com.typesafe.sbt.packager.debian.DebianPlugin.autoImport.DebianConstants._

enablePlugins(RunApplicationSettings, ExtensionPackaging, GitVersioning)

resolvers += "dnvriend" at "http://dl.bintray.com/dnvriend/maven"
libraryDependencies ++= Dependencies.dex

val packageSettings = Seq(
  maintainer := "wavesplatform.com",
  packageSummary := "DEX",
  packageDescription := s"Decentralized EXchange for Waves network. Compatible with ${nodeVersion.value} node version"
)

packageSettings
inScope(Global)(packageSettings)

Debian / maintainerScripts := maintainerScriptsAppend((Debian / maintainerScripts).value - Postrm)(
  Postrm ->
    s"""#!/bin/sh
       |set -e
       |if [ "$$1" = purge ]; then
       |  rm -rf /var/lib/${nodePackageName.value}/matcher
       |fi""".stripMargin
)
