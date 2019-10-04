resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "JBoss" at "https://repository.jboss.org",
  Resolver.sbtPluginRepo("releases")
)

Seq(
  "com.eed3si9n"      % "sbt-assembly"           % "0.14.5",
  "com.typesafe.sbt"  % "sbt-native-packager"    % "1.3.25",
  "org.scalastyle"    %% "scalastyle-sbt-plugin" % "1.0.0",
  "org.scoverage"     % "sbt-scoverage"          % "1.5.1",
  "se.marcuslonnberg" % "sbt-docker"             % "1.4.1",
  "com.typesafe.sbt"  % "sbt-git"                % "0.9.3",
  "com.lucidchart"    % "sbt-scalafmt"           % "1.15",
  "org.jetbrains"     % "sbt-ide-settings"       % "1.0.0"
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.9.9",
  "org.vafer"                    % "jdeb"                      % "1.5" artifacts Artifact("jdeb", "jar", "jar")
)
