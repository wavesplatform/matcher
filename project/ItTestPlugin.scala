import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import sbt.Keys._
import sbt.Tests.Group
import sbt._

// Separate projects for integration tests because of IDEA: https://youtrack.jetbrains.com/issue/SCL-14363#focus=streamItem-27-3061842.0-0
object ItTestPlugin extends AutoPlugin {

  private val PORT_RANGE_LENGTH = 50
  private val DEFAULT_PORT_RANGE = (30000, 65000)

  object autoImport extends ItKeys
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Test)(
      Seq(
        logDirectory := {
          val runId = Option(System.getenv("RUN_ID")).getOrElse {
            val formatter = DateTimeFormatter.ofPattern("MM-dd--HH_mm_ss")
            formatter.format(LocalDateTime.now()) // git branch?
          }
          val r = (Test / target).value / "logs" / runId
          IO.createDirectory(r)
          r
        },
        // Example: SCALATEST_EXCLUDE_TAGS="package1.Tag1 package2.Tag2 package3.Tag3"
        testOptions += {
          val sbtEnv = (Test / envVars).value

          val excludeTags = sys.env.get("SCALATEST_EXCLUDE_TAGS")
            .orElse(sbtEnv.get("SCALATEST_EXCLUDE_TAGS"))
            .fold(Seq.empty[String])(Seq("-l", _))

          val includeTags = sys.env.get("SCALATEST_INCLUDE_TAGS")
            .orElse(sbtEnv.get("SCALATEST_INCLUDE_TAGS"))
            .fold(Seq.empty[String])(Seq("-n", _))

          /* http://www.scalatest.org/user_guide/using_the_runner
           * f - select the file reporter with output directory
           * F - show full stack traces
           * W - without color
           * D - show all durations
           */
          val args = Seq("-fFWD", ((Test / logDirectory).value / "summary.log").toString) ++ excludeTags ++ includeTags
          Tests.Argument(TestFrameworks.ScalaTest, args: _*)
        },
        parallelExecution := true,
        javaOptions := { // TODO Doesn't work because this part of process is not forked
          val resourceDirectoryValue = (Test / resourceDirectory).value
          List(
            s"-Djava.util.logging.config.file=${resourceDirectoryValue / "jul.properties"}",
            s"-Dlogback.configurationFile=${resourceDirectoryValue / "logback-test.xml"}",
            "-Dwaves.it.logging.appender=FILE"
          ) ++ (Test / javaOptions).value
        },
        testGrouping := {
          val javaHomeValue = (test / javaHome).value
          val logDirectoryValue = (Test / logDirectory).value
          val envVarsValue = (Test / envVars).value
          val javaOptionsValue = (Test / javaOptions).value
          val (portRangeLowerBound, portRangeHigherBound) = sys.env.get("INTEGRATION_TESTS_PORT_RANGE").map { range =>
            val limits = range.split('-').map(_.toInt)
            if (limits.length != 2) throw new IllegalArgumentException(s"Illegal port range for tests! $range")
            val Array(first, second) = limits
            if (first >= second)
              throw new IllegalArgumentException(s"Illegal port range for tests! First boundary $first is bigger or equals second $second!")
            (first, second)
          }.getOrElse(DEFAULT_PORT_RANGE)
          val tests = (Test / definedTests).value

          // checks that we will not get higher than portRangeHigherBound
          if (tests.size * PORT_RANGE_LENGTH > portRangeHigherBound - portRangeLowerBound)
            throw new RuntimeException(
              s"""Cannot run tests;
                 |They need at least ${tests.size * PORT_RANGE_LENGTH} available ports,
                 | but specified interval has only ${portRangeHigherBound - portRangeLowerBound}
                 | """.stripMargin
            )

          tests.zipWithIndex.map { case (suite, i) =>
            val lowerBound = portRangeLowerBound + PORT_RANGE_LENGTH * i
            val higherBound = lowerBound + PORT_RANGE_LENGTH - 1
            Group(
              suite.name,
              Seq(suite),
              Tests.SubProcess(
                ForkOptions(
                  javaHome = javaHomeValue,
                  outputStrategy = (Test / outputStrategy).value,
                  bootJars = Vector.empty[java.io.File],
                  workingDirectory = Option((Test / baseDirectory).value),
                  runJVMOptions = Vector(
                    s"-Dwaves.it.logging.dir=${logDirectoryValue / suite.name.replaceAll("""(\w)\w*\.""", "$1.")}" // foo.bar.Baz -> f.b.Baz
                  ) ++ javaOptionsValue,
                  connectInput = false,
                  envVars = envVarsValue + ("TEST_PORT_RANGE" -> s"$lowerBound-$higherBound")
                )
              )
            )
          }
        }
      )
    )

}

trait ItKeys {
  val logDirectory = taskKey[File]("The directory where logs of integration tests are written")
}
