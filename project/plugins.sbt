resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "https://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org",
  Resolver.sbtPluginRepo("releases")
)

Seq(
  "com.thesamet"      % "sbt-protoc"             % "0.99.27",
  "org.jetbrains"     % "sbt-ide-settings"       % "1.0.0",
  "com.typesafe.sbt"  % "sbt-native-packager"    % "1.4.1",
  "org.scalastyle"    %% "scalastyle-sbt-plugin" % "1.0.0",
  "org.scoverage"     % "sbt-scoverage"          % "1.5.1",
  "se.marcuslonnberg" % "sbt-docker"             % "1.5.0",
  "com.typesafe.sbt"  % "sbt-git"                % "0.9.3",
  "org.scalameta"     % "sbt-scalafmt"           % "2.0.1",
  "ch.epfl.scala"     % "sbt-scalafix"           % "0.9.11",
  /*
  undeclaredCompileDependencies{test}
  unusedCompileDependencies{test}
   */
  "com.github.cb372" % "sbt-explicit-dependencies" % "0.2.10",
  // dependencyCheck to check against OWASP
  "net.vonbuchholtz" % "sbt-dependency-check" % "1.3.3",
  // dependencyUpdates
  "com.timushev.sbt" % "sbt-updates" % "0.5.0"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "org.vafer"            % "jdeb"            % "1.5" artifacts Artifact("jdeb", "jar", "jar"), // Required for "release" task
  "com.thesamet.scalapb" %% "compilerplugin" % "0.8.4"
)

addCompilerPlugin("org.scalamacros" % "paradise"        % "2.1.1" cross CrossVersion.full) // Remove after scala 2.13 migration
addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.10")
