resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "https://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org",
  Resolver.sbtPluginRepo("releases")
)

Seq(
  "com.thesamet" %% "sbt-protoc" % "1.0.6", // Have to wait NODE: https://github.com/wavesplatform/Waves/blob/master/project/plugins.sbt#L7
  "org.jetbrains" % "sbt-ide-settings" % "1.0.0",
  "com.typesafe.sbt" % "sbt-native-packager" % "1.4.1",
  "pl.project13.scala" % "sbt-jmh" % "0.3.7",
  "org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0",
  "org.scoverage" % "sbt-scoverage" % "1.5.1",
  "se.marcuslonnberg" % "sbt-docker" % "1.7.0",
  "com.typesafe.sbt" % "sbt-git" % "1.0.0",
  "org.scalameta" % "sbt-scalafmt" % "2.0.1",
  "ch.epfl.scala" % "sbt-scalafix" % "0.9.29",
  "com.github.tkawachi" % "sbt-repeat" % "0.1.0",
  /*
  undeclaredCompileDependencies{test}
  unusedCompileDependencies{test}
   */
  "com.github.cb372" % "sbt-explicit-dependencies" % "0.2.10",
  // dependencyCheck to check against OWASP
  "net.vonbuchholtz" % "sbt-dependency-check" % "1.3.3",
  // dependencyUpdates
  "com.timushev.sbt" % "sbt-updates" % "0.5.0",
  // metrics
  "com.lightbend.sbt" % "sbt-javaagent" % "0.1.6" // For release artifacts
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "org.vafer" % "jdeb" % "1.5" artifacts Artifact("jdeb", "jar", "jar"), // Required for "release" task
  // Have to wait NODE: https://github.com/wavesplatform/Waves/blob/master/project/plugins.sbt , also don't forget to update our jsonScalaPb
  "com.thesamet.scalapb" %% "compilerplugin" % "0.11.6",
  "commons-codec" % "commons-codec" % "1.14"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full) // Remove after scala 2.13 migration
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full)

addDependencyTreePlugin // https://github.com/sbt/sbt-dependency-graph
