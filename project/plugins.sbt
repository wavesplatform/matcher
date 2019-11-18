resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "https://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org",
  Resolver.sbtPluginRepo("releases")
)

Seq(
  "com.thesamet"      % "sbt-protoc"                % "0.99.19",
  "org.jetbrains"     % "sbt-ide-settings"          % "1.0.0",
  "com.eed3si9n"      % "sbt-assembly"              % "0.14.5",
  "com.typesafe.sbt"  % "sbt-native-packager"       % "1.4.1",
  "org.scalastyle"    %% "scalastyle-sbt-plugin"    % "1.0.0",
  "org.scoverage"     % "sbt-scoverage"             % "1.5.1",
  "se.marcuslonnberg" % "sbt-docker"                % "1.4.1",
  "com.typesafe.sbt"  % "sbt-git"                   % "0.9.3",
  "org.scalameta"     % "sbt-scalafmt"              % "2.0.1",
  "com.github.cb372"  % "sbt-explicit-dependencies" % "0.2.10"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.9.9",
  "org.vafer"                    % "jdeb"                      % "1.5" artifacts Artifact("jdeb", "jar", "jar"),
  "com.thesamet.scalapb"         %% "compilerplugin"           % "0.8.4"
)

addCompilerPlugin("org.scalamacros" % "paradise"        % "2.1.0" cross CrossVersion.full) // Remove after scala 2.13 migration
addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.6")
