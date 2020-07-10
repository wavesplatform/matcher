import com.typesafe.sbt.GitPlugin.autoImport.git
import sbt.Keys.isSnapshot
import sbt.{AutoPlugin, Def, PluginTrigger, Plugins, inTask, settingKey}
import sbtdocker.DockerPlugin.autoImport.{ImageName, docker, imageNames}

object ImageVersionPlugin extends AutoPlugin {

  object autoImport extends ImageVersionKeys
  import autoImport._

  override val trigger = PluginTrigger.NoTrigger

  override def requires: Plugins = sbtdocker.DockerPlugin

  override def projectSettings: Seq[Def.Setting[_]] = inTask(docker)(
    Seq(
      imageTagMakeFunction := (gitTag => gitTag),
      imageNames := {

        val latestImageName: ImageName  = ImageName(s"${nameOfImage.value}:latest")
        val maybeGitTag: Option[String] = git.gitDescribedVersion.value
        val mkTag: String => String     = imageTagMakeFunction.value

        if (!isSnapshot.value && maybeGitTag.isDefined) Seq(latestImageName, ImageName(s"${nameOfImage.value}:${mkTag(maybeGitTag.get)}"))
        else Seq(latestImageName)
      }
    )
  )
}

trait ImageVersionKeys {
  val nameOfImage          = settingKey[String]("Name of the image")
  val imageTagMakeFunction = settingKey[String => String]("Way to create image tag. Argument is the closest git tag")
}
