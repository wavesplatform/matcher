import java.nio.charset.StandardCharsets

import sbt.Keys.{sourceGenerators, sourceManaged, version}
import sbt.{AutoPlugin, Def, IO, _}

object VersionSourcePlugin extends AutoPlugin {

  object V {
    val scalaPackage = SettingKey[String]("version-scala-package", "Scala package name where Version object is created")
    val subProject = SettingKey[String]("version-sub-project", "Sub project name where Version object is created")
  }

  override def trigger: PluginTrigger = PluginTrigger.NoTrigger

  override def projectSettings: Seq[Def.Setting[_]] =
    (Compile / sourceGenerators) += Def.task {

      val versionFile = (Compile / sourceManaged).value / s"${V.scalaPackage.value.replace('.', '/')}/Version.scala"
      val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r

      val (major, minor, patch) = version.value match {
        case versionExtractor(ma, mi, pa) => (ma.toInt, mi.toInt, pa.toInt)
        case x =>
          // SBT downloads only the latest commit, so "version" doesn't know, which tag is the nearest
          if (Option(System.getenv("TRAVIS")).exists(_.toBoolean)) (0, 0, 0)
          else throw new IllegalStateException(s"${V.subProject.value}: can't parse version by git tag: $x")
      }

      IO.write(
        versionFile,
        s"""package ${V.scalaPackage.value}
           |
           |object Version {
           |  val VersionString = "${version.value}"
           |  val VersionTuple = ($major, $minor, $patch)
           |}
           |""".stripMargin,
        charset = StandardCharsets.UTF_8
      )

      Seq(versionFile)
    }

}
