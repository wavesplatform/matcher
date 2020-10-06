import com.typesafe.sbt.GitPlugin.autoImport.git
import sbt.Keys.isSnapshot
import sbt.{inTask, settingKey, AutoPlugin, Def, PluginTrigger, Plugins}
import sbtdocker.DockerPlugin.autoImport.{docker, imageNames, ImageName}

object ImageVersionPlugin extends AutoPlugin {

  object autoImport extends ImageVersionKeys
  import autoImport._

  override val trigger = PluginTrigger.NoTrigger

  override def requires: Plugins = sbtdocker.DockerPlugin

  override def projectSettings: Seq[Def.Setting[_]] = inTask(docker)(
    Seq(
      imageTagMakeFunction := (gitTag => gitTag),
      imageNames := {

        val latestImageName: ImageName = ImageName(s"${nameOfImage.value}:latest")
        val currentBranchName: String = git.gitCurrentBranch.value
        val currentBranchNameLowerCase: String = currentBranchName.toLowerCase
        val maybeGitTag: Option[String] = git.gitDescribedVersion.value
        val mkTag: String => String = imageTagMakeFunction.value
        val isRelease: Boolean = !isSnapshot.value && maybeGitTag.isDefined

        val mandatoryImageNames =
          if (currentBranchNameLowerCase.startsWith("dex-") || currentBranchNameLowerCase == "master")
            Seq(latestImageName, ImageName(s"${nameOfImage.value}:$currentBranchName"))
          else Seq(latestImageName)

        if (isRelease) mandatoryImageNames :+ ImageName(s"${nameOfImage.value}:${mkTag(maybeGitTag.get)}")
        else mandatoryImageNames
      }
    )
  )

}

trait ImageVersionKeys {
  val nameOfImage = settingKey[String]("Name of the image")
  val imageTagMakeFunction = settingKey[String => String]("Way to create image tag. Argument is the closest git tag")
}
