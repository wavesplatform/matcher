package load

import io.gatling.core.Predef._

class ConnectionsSimulation extends Simulation {

  val feeder = separatedValues(System.getProperty("ff"), ';').batch

  val scn = scenario("ConnectionsSimulation")
    .feed(feeder)
    .exec(connect)
    .repeat(System.getProperty("rt").toInt) {
      pingPong
    }
    .exec(close)

  setUp(
    scn.inject(atOnceUsers(System.getProperty("uc").toInt))
  )
}
