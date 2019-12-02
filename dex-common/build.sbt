val paradiseVersion = "2.1.1"

scalacOptions ++= {
  if (isPrior2_13(scalaVersion.value)) Nil else Seq("-Ymacro-annotations")
}

libraryDependencies ++= Dependencies.common ++ Seq(
  scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
  scalaOrganization.value % "scala-reflect"  % scalaVersion.value % Provided
) ++ (
  if (isPrior2_13(scalaVersion.value)) {
    Seq(
      compilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.patch)
    )
  } else Nil
)

def isPrior2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }
