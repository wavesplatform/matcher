enablePlugins(GatlingPlugin)

scalaVersion := "2.12.10"

scalacOptions := Seq(
  "-encoding", "UTF-8", "-target:jvm-1.8", "-deprecation",
  "-feature", "-unchecked", "-language:implicitConversions", "-language:postfixOps")

libraryDependencies += "io.gatling.highcharts" % "gatling-charts-highcharts" % "3.3.1" % "test,it"
libraryDependencies += "io.gatling"            % "gatling-test-framework"    % "3.3.1" % "test,it"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalaj" % "scalaj-http_2.11" % "2.3.0"



libraryDependencies += "com.wavesplatform" % "wavesj" % "0.16.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"